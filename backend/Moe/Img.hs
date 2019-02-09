{-# LANGUAGE DeriveGeneric #-}
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
