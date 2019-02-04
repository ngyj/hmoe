{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet

import Moe.Core (Moe)
------------------------------------------------------------------------------
data App = App
    { _moe   :: Snaplet Moe
    --, _heist :: Snaplet (Heist App)
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
