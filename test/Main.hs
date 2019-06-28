{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either (isLeft)

import Test.Hspec

import Moe.Utils
import Moe.InfoParser
import Moe.Img (Img(..))

main :: IO ()
main = hspec $ do
  describe "moe/utils" $ do
    describe "dropExt" dropExt_t
  describe "moe/infoparser" $ do
    describe "commas" commaP_t
    describe "source" srcP_t
    describe "wallpaper" wpP_t
    describe "filename" fnP_t
    describe "imageP" imP_t

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
  it "any/prefix" $ do
    parseOnly srcP "src = traceaksldf" `shouldBe` resL "traceaksldf"
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

imP_t = do
  let res = Right . Wp
  it "fn+src" $ do
    parseOnly imageP "[foo.png]\n src=https://foo.png" `shouldBe` Right (Img{imFn="foo.png"
                                                                            ,imCat=Nothing
                                                                            ,imSrc=Just "https://foo.png"
                                                                            ,imTag=[]
                                                                            ,imWp =[]})
  it "+cat" $ do
    parseOnly imageP "[foo.png]\n \nsrc=https://foo.png\n\ncat=bar"
      `shouldBe` Right (Img{imFn="foo.png"
                           ,imCat=Just "bar"
                           ,imSrc=Just "https://foo.png"
                           ,imTag=[]
                           ,imWp =[]})
  it "+cat" $ do
    parseOnly imageP "[foo.png]\n \nsrc=https://foo.png\n\ncat=bar\ntags=one,two,three"
      `shouldBe` Right (Img{imFn="foo.png"
                           ,imCat=Just "bar"
                           ,imSrc=Just "https://foo.png"
                           ,imTag=["one","two","three"]
                           ,imWp =[]})
