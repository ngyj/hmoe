{-# LANGUAGE DeriveGeneric #-}
module Moe.Img where

import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)
import Data.Text (Text)

data Img = Img
  { imFn  :: Text -- ^ image filename
  , imCat :: Maybe Text -- ^ image category
  , imSrc :: Maybe Text -- ^ image original source
  , imTag :: [Text] -- ^ image keywords/tags
  } deriving (Generic, Show, Eq)

instance ToJSON Img

imgSample :: [Img]
imgSample = [Img "foo.png" (Just "tan") Nothing []]
