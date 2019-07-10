{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.String (IsString)

-- | hostname of website there must be a better way, right?
host :: String
host = "https://haskell.moe/"


-- REFACTOR isn't making yet another type overkill?
-- | source services
data SrcServ = Trace | Iqdb deriving (Eq, Show)

-- | get the url for corresponding @SrcServ@
srcServUrl :: IsString s => SrcServ -> s
srcServUrl Trace = "https://trace.moe/?auto&url="
srcServUrl Iqdb  = "https://iqdb.org/?url="
