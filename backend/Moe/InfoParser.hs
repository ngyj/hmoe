{-# LANGUAGE OverloadedStrings #-}
module Moe.InfoParser ( module Moe.InfoParser
                      , parseOnly -- for debugging
                      ) where

import           Prelude hiding (takeWhile)

import           Control.Applicative (liftA2, (<|>), optional)
import           Data.Either (fromRight)
import           Data.Foldable (foldl')
import           Data.Functor (($>))
import           Data.Text as T (Text, strip, splitOn, null, lines, unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Debug.Trace (trace)

import           Data.Attoparsec.Text
import qualified Network.URI.Encode as Uri

import           Moe.Img
import           Moe.Utils (dropExt, Trie, prefixes)
import           Config (SrcServ(..))
import qualified Config as Cfg

infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)

newtype PFilename = Fn Text
                  deriving (Eq, Show)

data PField = Cat Text
            | Src (Either Text PSrc)
            | Tag [Text]
            | Wp Bool
            deriving (Eq, Show)

-- Currently the required tokens are the same as the SrcServ types
type PSrc = SrcServ

-- @TODO log error if parseImgs failed
-- | parse text from File
parseImages :: Trie Text -> Text -> Either String [Img]
parseImages wps txt =  map lookupWps <$> parseI txt
  where
    lookupWps i@Img{imFn=f} = i{imWp = prefixes (encodeUtf8 $ dropExt f) wps}
    parseI = parseOnly imageListP

imageListP:: Parser [Img]
imageListP = many' imageP

-- @TODO log when overwriting field, report line numbers?
-- SPEED use something other than linked lists for the fields?
-- | the @Img@ type builder
mkImg :: Text -> [PField] -> Img
mkImg fn = foldl' addField defaultImg
  where
    defaultImg = Img fn Nothing Nothing [] []
    escape = Uri.encodeText

    addField acc field = case field of
      Cat s -> acc{imCat=Just s}
      Src (Left s) -> acc{imSrc=Just s}
      Src (Right src) -> acc{imSrc=Just (Cfg.srcServUrl src)}
      Tag s -> acc{imTag=s}
      Wp _ -> acc

-- | the one image Parser
imageP :: Parser Img
imageP = do (Fn fn) <- fnP
            fields <- many' $ choice [srcP, catP, tagP, wpP]
            optional eolof
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
srcP = Src
       <$> kvPairP (string "source" <|> string "src")
                   ( Right <$> (try tokens <* endOfField)
                   <|> Left <$> (takeTill isEndOfWord <* endOfField))
       <?> "source"
  where
    srcName :: Parser Text
    srcName = char ':' *> takeTill isEndOfWord

    tokens :: Parser PSrc
    tokens = do { string "trace"; optional (string ".moe"); Trace <$> srcName }
             <|> do { string "iqdb";  Iqdb <$> srcName }

catP :: Parser PField
catP = Cat . strip
       <$> kvPairP (string "category" <|> string "cat")
                   (takeTill isEndOfWord <* endOfField)
       <?> "category"

tagP :: Parser PField
tagP = Tag
       <$> kvPairP (string "tags")
                   commaP
       <?> "tags"

wpP :: Parser PField
wpP = Wp
      <$> kvPairP (string "wp" <|> string "wallpaper")
                  (boolP <* endOfField)
      <?> "wallpaper"

-- ** helpers
boolP :: Parser Bool
boolP = (asciiCI "true" $> True)
        <|> (asciiCI "false" $> False)
        <?> "bool"

-- | key value pair (separated by '=') parser
kvPairP :: Parser Text -> Parser a -> Parser a
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

-- | End Of Line Or File
eolof :: Parser ()
eolof = endOfLine <|> endOfInput

-- | skips horizontal space and eats EOL or EOF
endOfField :: Parser ()
endOfField = skipHSpace <* eolof

-- | skip horizontal space
skipHSpace :: Parser ()
skipHSpace = skipWhile isHorizontalSpace

-- | skip all whitespace (including newlines)
skipVSpace :: Parser ()
skipVSpace = skipSpace
