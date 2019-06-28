{-# LANGUAGE OverloadedStrings #-}
module Moe.InfoParser ( module Moe.InfoParser
                      , parseOnly -- for debugging
                      ) where

import           Prelude hiding (takeWhile)

import           Control.Applicative (liftA2, (<|>), optional)
import           Data.Either (fromRight)
import           Data.Functor (($>))
import           Data.Text as T (Text, strip, splitOn, null, lines, unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Debug.Trace (trace)

import           Data.Attoparsec.Text

import           Moe.Img
import           Moe.Utils (dropExt, Trie, prefixes)
import qualified Config

infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)

newtype PFilename = Fn Text
                  deriving (Eq, Show)

data PField = Cat Text
            | Src (Either Text PSrcToken)
            | Tag [Text]
            | Wp Bool
            deriving (Eq, Show)

data PSrcToken = Trace | Iqdb deriving (Eq, Show)

-- | parse text from File
parseImages :: Trie Text -> Text -> [Img]
parseImages wps = map lookupWps . parseImgs
  where
    lookupWps i@Img{imFn=f} = i{imWp = prefixes (encodeUtf8 $ dropExt f) wps}
    parseImgs = fromRight [] . parseOnly (many' imageP)

-- REFACTOR Lenses and folds, please
-- TODO log when overwriting field, report line numbers?
-- | the @Img@ type builder
mkImg :: Text -> [PField] -> Img
mkImg fn = go (defaultImg fn)
  where
    defaultImg s = Img s Nothing Nothing [] []
    url = T.pack Config.host <> "moe/" <> fn
    escape = id

    go acc [] = acc
    go acc (x:xs) = case x of
      Cat s -> go acc{imCat=Just s} xs
      Src (Left s) -> go acc{imSrc=Just s} xs
      Src (Right Trace) -> go acc{imSrc=Just ("https://trace.moe/?auto&url=" <> escape fn)} xs
      Src (Right Iqdb) -> go acc{imSrc=Just ("https://iqdb.org/?url=" <> escape fn)} xs
      Tag s -> go acc{imTag=s} xs
      Wp _ -> go acc xs
-- error "image_info format error: should start with a filename"

-- | the one image Parser
imageP :: Parser Img
imageP = do (Fn fn) <- fnP
            fields <- many' $ choice [srcP, catP, tagP, wpP]
            eolof
            pure $ mkImg fn fields

-- ** field parsers
fnP :: Parser PFilename
fnP = do skipHSpace
         _ <- char '['
         fn <- takeTill $ liftA2 (||) (==']') isEndOfLine
         _ <- char ']'
         return . Fn . strip $ fn
      <?> "filename"

srcP :: Parser PField
srcP = kvPairP (string "source" <|> string "src")
               (Src <$> ((try keywords <* endOfField) <|> Left <$> other))
       <?> "source"
  where
    keywords = Right <$> do Trace <$ string "trace" <* optional (string ".moe")
                              <|> Iqdb <$ string "iqdb.org"
    other = takeWhile (not . isEndOfWord) <* endOfField

catP :: Parser PField
catP = kvPairP (string "category" <|> string "cat")
               (Cat . strip <$> takeTill isEndOfWord <* endOfField)
       <?> "category"

tagP :: Parser PField
tagP = kvPairP (string "tags")
               (Tag <$> commaP)
       <?> "tags"
  where
    -- REFACTOR delete this
    splitTags = filter (not . T.null) . map strip . splitOn "," . strip

wpP :: Parser PField
wpP = kvPairP (string "wp" <|> string "wallpaper")
              (Wp <$> boolP <* endOfField)
      <?> "wallpaper"

-- ** helpers
boolP :: Parser Bool
boolP = (asciiCI "true" $> True)
        <|> (asciiCI "false" $> False)
        <?> "bool"

-- | key value pair (separated by '=') parser
kvPairP :: Parser Text -> Parser PField -> Parser PField
kvPairP keyP valueP = skipVSpace
  *> keyP
  *> skipVSpace *> char '='
  *> skipVSpace
  *> valueP

-- | parses comma separated words accross newlines
-- @a\n, b@ or @a\n  , b@
commaP :: Parser [Text]
commaP = do x <- skipVSpace *> takeTill end <* skipHSpace
            xs <- try (comma *> commaP)
                  <|> ([] <$ eolof)
            pure (x:xs)
  where
    comma = char ',' <|> (endOfLine *> skipVSpace *> char ',')
    end c = c == ',' || isEndOfLine c || isHorizontalSpace c

-- | end of word
isEndOfWord :: Char -> Bool
isEndOfWord c = isHorizontalSpace c || isEndOfLine c

-- | end of line or file
eolof :: Parser ()
eolof = endOfLine <|> endOfInput

endOfField :: Parser ()
endOfField = skipHSpace <* eolof

-- | skip horizontal space
skipHSpace = skipWhile isHorizontalSpace

-- | skip all whitespace (including newlines)
skipVSpace = skipSpace
