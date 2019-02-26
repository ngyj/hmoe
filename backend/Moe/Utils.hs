{-# LANGUAGE GADTs #-}
module Moe.Utils where

import           Control.Monad ((>=>))
import           Data.Maybe (listToMaybe, fromJust, fromMaybe)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Snap.Core (MonadSnap, getRequest, rqParam)

-- * Snap
-- | get the Parameter the head of parameter's values from the Request in the monad
getParam :: MonadSnap m => ByteString -> m (Maybe ByteString)
getParam s = (rqParam s >=> listToMaybe) <$> getRequest

-- | get the parameter's values list
getParams :: MonadSnap m => ByteString -> m [ByteString]
getParams s = fromMaybe [] . rqParam s <$> getRequest

-- | uses head and fromJust
unsafeGetParam :: MonadSnap m => ByteString -> m ByteString
unsafeGetParam s = head . fromJust . rqParam s <$> getRequest

-- -----------------------------------------------------------------------------
-- * Misc.
-- | IsString-like with toString function instead
class ToString a where
  toString :: a -> String

instance (a ~ Char) => ToString [a] where
  toString = id

instance ToString B.ByteString where
  toString = B.unpack

instance ToString BL.ByteString where
  toString = BL.unpack

instance ToString T.Text where
  toString = T.unpack

instance ToString TL.Text where
  toString = TL.unpack
