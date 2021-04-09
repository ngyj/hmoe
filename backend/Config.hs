{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.String (IsString)

-- | hostname of website
-- @IMPROVEMENT feels like a hack, maybe just query the ip on the system?
host :: String
host = "https://" <> ['h', 'a', 's', 'k', 'e', 'l', 'l'] <> ".moe/"


-- | source services
data SrcServ = Trace | Iqdb deriving (Eq, Show)

-- | get the url for corresponding @SrcServ@
srcServUrl :: IsString s => SrcServ -> s
srcServUrl Trace = "https://trace.moe/?auto&url="
srcServUrl Iqdb  = "https://iqdb.org/?url="
