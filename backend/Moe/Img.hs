{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Moe.Img where

import Control.Applicative ((<*))
import Debug.Trace (traceShowM)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString, isSuffixOf)
import Graphics.GD

import Moe.Utils

-- TODO imWp :: [Text] in case we have more than one wp?
data Img = Img
  { imFn  :: Text -- ^ image filename
  , imCat :: Maybe Text -- ^ image category
  , imSrc :: Maybe Text -- ^ image original source
  , imTag :: [Text] -- ^ image keywords/tags
  , imWp  :: [Text] -- ^ wallpaper sized image in img/wp/{imFn} ?
  } deriving (Generic, Show, Eq)

instance ToJSON Img

imgSample :: [Img]
imgSample = [Img "foo.png" (Just "tan") Nothing [] []]

maxTWidth :: Int
maxTWidth = 150

writeThumb :: Image -> IO Image
writeThumb src = do (w, h) <- imageSize src
                    let h' = min h w
                    let scaledHeight = ceiling @Double @Int
                                       $ fromIntegral (h' * maxTWidth) / fromIntegral w
                    img <- newImage (maxTWidth, scaledHeight)
                    copyRegionScaled (0,0) (w, h') src (0,0) (maxTWidth, scaledHeight) img
                    return img

mkThumbnail :: ByteString -> IO ()
mkThumbnail fn | extension [".png"] = go (loadPngFile, savePngFile)
               | extension [".jpg", ".jpeg"] = go (loadJpegFile, saveJpegFile (-1))
               | extension [".gif"] = go (loadGifFile, saveGifFile)
               | otherwise = return () -- FIXME log this (?)
  where
    extension = any (`isSuffixOf` fn)
    go (load, save) = load (inImgDir fn)
                      >>= writeThumb
                      >>= save (inThumbDir fn)
                      <* traceShowM ("mkThumbnail: " <> fn)

inImgDir, inThumbDir :: ToString a => a -> String
inImgDir = ("static/img/" ++) . toString
inThumbDir = ("static/img/thumb/" ++) . toString
