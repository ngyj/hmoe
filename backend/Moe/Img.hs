{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Moe.Img where

import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)
import Data.Text (Text)

-- TODO imWp :: [Text] in case we have more than one wp?
data Img = Img
  { imFn  :: Text -- ^ image filename
  , imCat :: Maybe Text -- ^ image category
  , imSrc :: Maybe Text -- ^ image original source
  , imTag :: [Text] -- ^ image keywords/tags
  , imWp  :: Bool -- ^ wallpaper sized image in img/wp/{imFn} ?
  } deriving (Generic, Show, Eq)

instance ToJSON Img

imgSample :: [Img]
imgSample = [Img "foo.png" (Just "tan") Nothing [] False]


type Point = (Int, Int)
type Size = (Int, Int)
data Im = Im
copyRegionScaled :: Point -> Size -> Im -> Point -> Size -> Im
copyRegionScaled = undefined
maxWidth = 300
thumb :: Im -> Im
thumb im = copyRegionScaled (0,0) (w, h') im (0,0) (maxWidth, scaledHeight)
  where
    (w, h) = (0, 0)
    h' = min h w
    scaledHeight = ceiling @Double @Int
                   $ fromIntegral (h' * maxWidth) / fromIntegral w
