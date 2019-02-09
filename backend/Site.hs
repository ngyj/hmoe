{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe (serveDirectory)

import           Application
import           Moe.Api (moeInit)

handleFoo :: Handler App App ()
handleFoo = do
  mid <- getParam "id"
  writeBS $ maybe "foo" id mid

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("foo", handleFoo)
         , ("",    serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    a <- nestSnaplet "moe" moe moeInit

    addRoutes routes
    return $ App a
