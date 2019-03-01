{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Moe.Utils ( module Moe.Utils
                 , Trie
                 )where

import           Control.Monad ((>=>))
import           Data.Maybe (listToMaybe, fromJust, fromMaybe)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Snap.Core (MonadSnap, getRequest, rqParam)
import           Data.Trie (fromList, Trie, submap, elems)

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
-- | build the trie from a @[String]@
mkTrie :: (String -> a) -> [String] -> Trie a
mkTrie f = fromList . map (\s -> (B.pack s, f s))

-- | get all elements matching the prefix
prefixes :: ByteString -> Trie a -> [a]
prefixes s = elems . submap s

-- FIXME match on list of known extensions?
-- | get the filename without the extension
dropExt :: Text -> Text
dropExt s = case T.breakOnEnd "." s of
      ("", _) -> s
      (".", _) -> s
      (f, _) -> T.dropEnd 1 f

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
