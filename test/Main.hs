{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either (isLeft, isRight, fromRight, fromLeft)

import Test.Hspec

import Moe.Utils
import Moe.InfoParser
import Moe.Img (Img(..))

main :: IO ()
main = hspec $ do
  -- REFACTOR separate module for backend & parse/utils/other
  describe "moe/utils" $ do
    describe "dropExt" dropExt_t
  describe "moe/infoparser" $ do
    describe "commas" commaP_t
    describe "source" srcP_t
    describe "wallpaper" wpP_t
    describe "filename" fnP_t
    describe "imageP" imP_t
    describe "imageListP" imListP_t
    -- TODO : wallpapers,

dropExt_t = do
  it "normal" $ do
    dropExt "file.ext" `shouldBe` "file"
  it "dots" $ do
    dropExt "some.file.ext" `shouldBe` "some.file"
  it "hidden" $ do
    dropExt ".hidden.ext" `shouldBe` ".hidden"
  it "hidden/no-ext" $ do
    dropExt ".hidden" `shouldBe` ".hidden"
  it "no-ext" $ do
    dropExt "somefile" `shouldBe` "somefile"

commaP_t = do
  it "simple" $ do
    parseOnly commaP "a,b,c\n" `shouldBe` Right ["a","b","c"]
  it "spaces" $ do
    parseOnly commaP "a,\nb,\n   c,\n  d\n" `shouldBe` Right ["a","b","c","d"]
  it "newlines" $ do
    parseOnly commaP "a\n  ,b\n,  c\n,  d\n" `shouldBe` Right ["a","b","c","d"]
  it "mix" $ do
    parseOnly commaP "a   \n  , b  ,  c ,  \n  d\n" `shouldBe` Right ["a","b","c","d"]

srcP_t = do
  let resL = Right . Src . Left
  let resR = Right . Src . Right
  it "simple" $ do
    parseOnly srcP "src=https://foo.com" `shouldBe` resL "https://foo.com"
  it "long" $ do
    parseOnly srcP "source=https://foo.com" `shouldBe` resL "https://foo.com"
  it "space" $ do
    parseOnly srcP " src  =   https://foo.com\n" `shouldBe` resL "https://foo.com"
  it "trace.moe" $ do
    parseOnly srcP "src = trace.moe" `shouldBe` resR Trace
  it "trace" $ do
    parseOnly srcP "src = trace" `shouldBe` resR Trace
  it "iqdb" $ do
    parseOnly srcP "src = iqdb.org" `shouldBe` resR Iqdb
  it "trace/prefix" $ do
    parseOnly srcP "src = traceaksldf" `shouldBe` resL "traceaksldf"
  -- possible clash between key for parser and value
  it "clash/source" $ do
    parseOnly srcP "src = source" `shouldBe` resL "source"
  it "clash/src" $ do
    parseOnly srcP "src = src" `shouldBe` resL "src"
  it "clash/src-post" $ do
    parseOnly srcP "src = srcgarbage" `shouldBe` resL "srcgarbage"
  it "fail/prefix" $ do
    parseOnly srcP "src = iqdb.org adf" `shouldSatisfy` isLeft
  it "fail" $ do
    parseOnly srcP "src = aklasfjal adf" `shouldSatisfy` isLeft

fnP_t = do
  let res = Right . Fn
  it "simple" $ do
    parseOnly fnP "[foo.png]" `shouldBe` res "foo.png"
  it "spaces" $ do
    parseOnly fnP " [    foo.png  ] \n" `shouldBe` res "foo.png"
  it "fail/newline" $ do
    parseOnly fnP " [    foo.png\n  ] \n" `shouldSatisfy` isLeft
  it "fail/opener" $ do
    parseOnly fnP "    foo.png  ] \n" `shouldSatisfy` isLeft
  it "fail/closing" $ do
    parseOnly fnP "[    foo.png  " `shouldSatisfy` isLeft
  it "fail/closing'" $ do
    parseOnly fnP "[    foo.png  \n" `shouldSatisfy` isLeft

wpP_t = do
  let res = Right . Wp
  it "true" $ do
    parseOnly wpP "wallpaper = true" `shouldBe` res True
  it "false" $ do
    parseOnly wpP "   wp = false\n" `shouldBe` res False
  it "newline/after" $ do
    parseOnly wpP "wp = \n true" `shouldBe` res True
  it "newline/before" $ do
    parseOnly wpP "wp \n = true" `shouldBe` res True
  it "false/space" $ do
    parseOnly wpP "   wp = false  \n" `shouldBe` res False
  it "fail/1" $ do
    parseOnly wpP "wp = 1" `shouldSatisfy` isLeft

img = Img{ imFn  = ""
         , imCat = Nothing
         , imSrc = Nothing
         , imTag = []
         , imWp  = []
         }

imP_t = do
  it "fn+src" $ do
    parseOnly imageP "[foo.png]\n src=https://foo.png" `shouldBe` Right img{imFn="foo.png"
                                                                           ,imSrc=Just "https://foo.png"
                                                                           }
  it "^+cat" $ do
    parseOnly imageP "[foo.png]\n \nsrc=https://foo.png\n\ncat=bar"
      `shouldBe` Right img{imFn="foo.png"
                          ,imCat=Just "bar"
                          ,imSrc=Just "https://foo.png"
                          }
  it "^+tags" $ do
    parseOnly imageP "[foo.png]\n \nsrc=https://foo.png\n\ncat=bar\ntags=one,two,three"
      `shouldBe` Right img{imFn="foo.png"
                          ,imCat=Just "bar"
                          ,imSrc=Just "https://foo.png"
                          ,imTag=["one","two","three"]
                          }
  it "dup_field" $ do
    parseOnly imageP "[foo.png]\n cat=bar \n cat=baz \n"
      `shouldBe` Right img{imFn="foo.png"
                          ,imCat=Just "baz"
                          }

testT1 =  "[xxx.png]\nsrc=https://xxx.com\ncat=cat\ntags=one,two,three"
testT2 =  "[yyy.png]\nsrc=https://yyy.net\nwp=true\ntags=four,five,six"
testI = fromRight img . parseOnly imageP

imListP_t = do
  it "null" $ do
    parseOnly (imageListP) "" `shouldBe` Right []
  it "one" $ do
    parseOnly (imageListP) testT1 `shouldBe` Right [testI testT1]
  it "two" $ do
    parseOnly (imageListP) (testT1<>"\n"<>testT2) `shouldBe` Right [testI testT1, testI testT2]
  it "two_extra" $ do
    parseOnly (imageListP) (testT1<>"\n\n"<>testT2) `shouldBe` Right [testI testT1, testI testT2]

  it "fail/separation" $ do
    parseOnly (imageListP) (testT1<>testT2) `shouldBe` Right [(testI testT2){imFn = imFn (testI testT1)
                                                                            ,imCat = imCat (testI testT1)}]
  it "fail/second" $ do
    parseOnly (imageListP) (testT1<>"\n[as\n"<>testT2) `shouldBe` Right [testI testT1]
