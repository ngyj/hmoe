{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Moe.Utils

main :: IO ()
main = hspec $ do
  describe "moe/utils" $ do
    describe "moe/utils/dropExt" $ do
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
