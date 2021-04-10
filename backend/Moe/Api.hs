{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Moe.Api where

import           Data.Maybe (fromJust)
import           Data.String (fromString)
import           Control.Applicative ((<|>))
import           Control.Monad (unless, liftM2)
import           Control.Monad.IO.Class

import           Data.Aeson (encode, ToJSON(..), (.=), object, pairs)
import           Data.ByteString (ByteString, pack)
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import           Snap.Core hiding (Response)
import           Snap.Snaplet
import           Snap.Util.FileServe (serveFile, serveDirectory)
import           System.Directory (doesFileExist, getModificationTime, listDirectory)

import           Moe.Img (Img, inImgDir, inThumbDir, mkThumbnail)
import           Moe.InfoParser (parseImages)
import           Moe.Utils (unsafeGetParam, mkTrie)
import           Config

data Moe = Moe

instance Show Moe where
  show Moe = "moe"

routeTxt = T.pack $ show Moe

moeRoutes :: [(ByteString, Handler a Moe ())]
moeRoutes = [("status"   , method GET rOK)
            ,("wp"       , serveDirectory $ inImgDir @String "/wp")
            ,("src"      , serveDirectory $ inImgDir @String "/src")
            ,("moelist"  , method GET rImgList)
            ,(":filename", method GET rImg)
            ,(""         , method GET rImgList)
            ]

rOK :: Handler a Moe ()
rOK = modifyResponse $ setResponseCode 200

rImgList :: Handler a Moe ()
rImgList = do modifyResponse $ setHeader "content-type" "application/json"
              liftIO imgs >>= \case Left e -> logError (fromString e)
                                    Right (ws, is) -> mapM logWarning ws >> (writeLBS . encode . OK) is
                where
                  logWarning = logError . ("Warning: " <>)
                  wps = mkTrie T.pack <$> listDirectory (inImgDir @String "wp")
                  -- pass the walpaper with the file
                  imgs = parseImages <$> wps <*> T.readFile "static/img_info.txt"

notFound :: String
notFound = "static/404.png"

rImg :: Handler a Moe ()
rImg = do f <- unsafeGetParam "filename"
          e <- liftIO $ doesFileExist (inImgDir f)
          if not e then
            serveFile notFound
          else do
            ps <- getParams
            case ps !? "thumbnail" of
              Just _ -> serveThumbnail f
              Nothing -> serveFile (inImgDir f)
  where
    serveThumbnail f = do liftIO $ coherent f >>= flip unless (mkThumbnail f)
                          serveFile (inThumbDir f)

    (<&&>) ma mb = ma >>= \a -> if a then mb else pure a
    coherent f = doesFileExist (inThumbDir f)
                 <&&> liftM2 (<) (getModificationTime $ inImgDir f) (getModificationTime $ inThumbDir f)

    redirectTrace _ = pure ()

moeInit :: SnapletInit a Moe
moeInit = makeSnaplet routeTxt "hmoe apis" Nothing $ do
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
