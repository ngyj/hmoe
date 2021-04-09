{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.String (IsString(..))
import Data.Text (Text, intercalate)

-- | hostname of website
-- @IMPROVEMENT feels like a hack, maybe just query the ip on the system?
host :: String
host = ['h', 'a', 's', 'k', 'e', 'l', 'l'] <> ".moe"


-- | source services
data SrcServ = Trace Text | Iqdb Text deriving (Eq, Show)

srcUrl :: SrcServ -> Text
srcUrl (Trace s) = s
srcUrl (Iqdb s) = s

-- | get the url for corresponding @SrcServ@
srcServUrl :: SrcServ -> Text
srcServUrl ss = 
  let url s = intercalate "/" ["http:/", fromString host, "moe/src", s] in
  case ss of 
    Trace s ->  "https://trace.moe/?auto&url=" <> url s
    Iqdb s -> "https://iqdb.org/?url=" <> url s

