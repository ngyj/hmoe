module Moe.Api where

import           Control.Monad.IO.Class

import           Data.Aeson (encode)
import           Data.ByteString (ByteString)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)
import           Snap.Core
import           Snap.Snaplet

import           Moe.InfoParser (parseLines)

data Moe = Moe

moeRoutes :: [(ByteString, Handler a Moe ())]
moeRoutes = [("status", method GET rOK)
            ,("/", method GET rImg)]

rOK :: Handler a Moe ()
rOK = modifyResponse $ setResponseCode 200

rImg :: Handler a Moe ()
rImg = do modifyResponse $ setHeader "content-type" "application/json"
          liftIO imgs >>= writeLBS . encode
  where
    imgs = parseLines . T.lines <$> T.readFile "static/img_info.txt"

moeInit :: SnapletInit a Moe
moeInit = makeSnaplet "moe" "haskell.moe apis" Nothing $ do
  addRoutes moeRoutes
  return Moe
