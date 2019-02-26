{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Moe.Api where

import           Control.Monad (unless, liftM2)
import           Control.Monad.IO.Class

import           Data.Aeson (encode, ToJSON(..), (.=), object, pairs)
import           Data.ByteString (ByteString)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)
import           Snap.Core hiding (Response)
import           Snap.Snaplet
import           Snap.Util.FileServe (serveFile)
import           System.Directory (doesFileExist, getModificationTime)

import           Moe.Img (Img, inImgDir, inThumbDir, mkThumbnail)
import           Moe.InfoParser (parseLines)
import           Moe.Utils (unsafeGetParam)

data Moe = Moe

moeRoutes :: [(ByteString, Handler a Moe ())]
moeRoutes = [("status"   , method GET rOK)
            ,("moelist"  , method GET rImgList)
            ,(":filename", method GET rImg)
            ,(""         , method GET rImgList)
            ]

rOK :: Handler a Moe ()
rOK = modifyResponse $ setResponseCode 200

rImgList :: Handler a Moe ()
rImgList = do modifyResponse $ setHeader "content-type" "application/json"
              liftIO imgs >>= writeLBS . encode . OK
                where
                  imgs = parseLines . T.lines <$> T.readFile "static/img_info.txt"

notFound :: String
notFound = "static/404.png"

rImg :: Handler a Moe ()
rImg = do f <- unsafeGetParam "filename"
          e <- liftIO $ doesFileExist (inImgDir f)
          if not e then
            serveFile notFound
          else
            getParam "thumbnail"
            >>= \case Nothing -> serveFile (inImgDir f)
                      Just _  -> serveThumbnail f
  where
    serveThumbnail f = do liftIO $ coherent f >>= flip unless (mkThumbnail f)
                          serveFile (inThumbDir f)

    (<&&>) ma mb = ma >>= \a -> if a then mb else ma
    coherent f = doesFileExist (inThumbDir f)
                 <&&> liftM2 (<) (getModificationTime $ inImgDir f) (getModificationTime $ inThumbDir f)

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
