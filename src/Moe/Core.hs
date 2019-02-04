module Moe.Core where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Snap.Core
import Snap.Snaplet

import Moe.Img

data Moe = Moe

moeRoutes :: [(ByteString, Handler a Moe ())]
moeRoutes = [("status", method GET rOK)
            ,("/", method GET rImg)]

rOK :: Handler a Moe ()
rOK = modifyResponse $ setResponseCode 200

rImg :: Handler a Moe ()
rImg = do modifyResponse $ setHeader "content-type" "application/json"
          writeLBS . encode $ imgSample

moeInit :: SnapletInit a Moe
moeInit = makeSnaplet "moe" "haskell.moe apis" Nothing $ do
  addRoutes moeRoutes
  return Moe
