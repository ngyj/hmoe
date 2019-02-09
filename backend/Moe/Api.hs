module Moe.Api where

import           Control.Monad.IO.Class

import           Data.Aeson (encode, ToJSON(..), (.=), object, pairs)
import           Data.ByteString (ByteString)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)
import           Snap.Core hiding (Response)
import           Snap.Snaplet

import           Moe.Img (Img)
import           Moe.InfoParser (parseLines)

data Moe = Moe

moeRoutes :: [(ByteString, Handler a Moe ())]
moeRoutes = [("status", method GET rOK)
            ,("/", method GET rImg)]

rOK :: Handler a Moe ()
rOK = modifyResponse $ setResponseCode 200

rImg :: Handler a Moe ()
rImg = do modifyResponse $ setHeader "content-type" "application/json"
          liftIO imgs >>= writeLBS . encode . OK
  where
    imgs = parseLines . T.lines <$> T.readFile "static/img_info.txt"

moeInit :: SnapletInit a Moe
moeInit = makeSnaplet "moe" "hmoe apis" Nothing $ do
  addRoutes moeRoutes
  return Moe

data Response = OK [Img]
              | Error String
              deriving Show

instance ToJSON Response where
  toJSON (OK ls)  = object ["data" .= ls, "error" .= (Nothing :: Maybe String)]
  toJSON (Error s) = object [ "data" .= ([] ::[Img]), "error" .= Just s]

  toEncoding (OK ls)  = pairs ("data" .= ls <> "error" .= (Nothing :: Maybe String))
  toEncoding (Error s) = pairs ("data" .= ([] :: [Img]) <> "error" .= Just s)
