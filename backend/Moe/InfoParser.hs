{-# LANGUAGE OverloadedStrings
           , BangPatterns
           , LambdaCase #-}
module Moe.InfoParser ( module Moe.InfoParser
                      , parseOnly -- for debugging
                      ) where

import           Prelude hiding (takeWhile)

import           Control.Monad (foldM)
import           Control.Applicative (liftA2, (<|>), optional)
import           Data.ByteString (ByteString)
import           Data.Either (fromRight)
import           Data.Foldable (foldl')
import           Data.Functor (($>))
import           Data.Text as T (Text, strip, splitOn, null, lines, unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Debug.Trace (trace)

import           Data.Attoparsec.Text
import qualified Network.URI.Encode as Uri
import           Control.Monad.Trans.Writer.Strict (Writer, writer, tell, runWriter)

import           Moe.Img
import           Moe.Utils (dropExt, Trie, prefixes)
import           Config (SrcServ(..))
import qualified Config as Cfg

infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)
infixr 2 <||>
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

newtype PFilename = Fn Text
                  deriving (Eq, Show)

data PField = Cat Text
            | Src (Either Text PSrc)
            | Tag [Text]
            | Wp Bool
            | Unknown (Text, Text)
            deriving (Eq, Show)

type Warnings = [ByteString]

-- Currently the required tokens are the same as the SrcServ types
type PSrc = SrcServ

-- TODO log error if parseImgs failed
-- | parse text from File
parseImages :: Trie Text -> Text -> Either String (Warnings, [Img])
parseImages wps txt =  (\case (!ws, !is) -> (ws, map lookupWps is)) <$> parseI txt
  where
    lookupWps i@Img{imFn=f} = i{imWp = prefixes (encodeUtf8 $ dropExt f) wps}
    parseI = parseOnly imageListP

imageListP:: Parser (Warnings, [Img])
imageListP = sequence <$> many' imageP

-- TODO log when overwriting field, report line numbers?
-- SPEED use something other than linked lists for the fields?
-- FIXME this is just Writer
-- | the @Img@ type builder
mkImg :: Text -> [PField] -> (Warnings, Img)
mkImg fn = swap . runWriter . foldM addField defaultImg
  where
    swap (a, b) = (b, a)
    defaultImg = Img fn Nothing Nothing [] []
    escape = Uri.encodeText

    addField :: (Img -> PField -> Writer [ByteString] Img)
    addField img field = 
      case field of
        Cat s           -> pure img{imCat=Just s}
        Src (Left s)    -> pure img{imSrc=Just s}
        Src (Right src) -> pure img{imSrc=Just (Cfg.srcServUrl src)}
        Tag s           -> pure img{imTag=s}
        Wp _            -> pure img  -- FIXME
        Unknown (k, _)  -> let w = "unknown field: `" <> encodeUtf8 k <> "` in [" <> encodeUtf8 fn <> "]."
                           in seq w (tell [w]) >> pure img

-- | the one image Parser
imageP :: Parser (Warnings, Img)
imageP = do (Fn fn) <- fnP
            fields <- many' $ choice [catP, srcP, tagP, wpP, unknownP]
            optional eolof
            pure $ mkImg fn fields

-- ** field parsers
fnP :: Parser PFilename
fnP = do skipHSpace
         _ <- char '['
         fn <- takeTill (== ']')
         _ <- char ']'
         skipHSpace
         return . Fn . strip $ fn
      <?> "filename"

srcP :: Parser PField
srcP = Src
       <$> kvPairP' (string "source" <|> string "src")
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
       <$> kvPairP' (string "category" <|> string "cat")
                    (lineP <* endOfField)
       <?> "category"

tagP :: Parser PField
tagP = Tag
       <$> kvPairP' (string "tags")
                    commaP
       <?> "tags"

wpP :: Parser PField
wpP = Wp
      <$> kvPairP' (string "wp" <|> string "wallpaper")
                   (boolP <* endOfField)
      <?> "wallpaper"

unknownP :: Parser PField
unknownP = Unknown
        <$> kvPairP (takeTill (isChar '=' <||> isEndOfWord))
                    (lineP <* endOfField)
        <?> "unknown field"


-- ** helpers
boolP :: Parser Bool
boolP = (asciiCI "true" $> True)
        <|> (asciiCI "false" $> False)
        <?> "bool"

-- | key value pair (separated by '=') parser
kvPairP :: Parser Text -> Parser a -> Parser (Text, a)
kvPairP keyP valueP =
  do skipVSpace
     k <- keyP
     skipVSpace *> char '='
     skipVSpace
     v <- valueP
     pure (k, v)

-- | key value pair (separated by '=') parser, returns only the pair
kvPairP' :: Parser Text -> Parser a -> Parser a
kvPairP' keyP valueP = snd <$> kvPairP keyP valueP

-- | parses text until endOfLine
lineP :: Parser Text
lineP = strip <$> takeTill isEndOfLine

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

isChar :: Char -> Char -> Bool
{-# INLINE isChar #-}
isChar = (==)

-- | end of word
isEndOfWord :: Char -> Bool
{-# INLINE isEndOfWord #-}
isEndOfWord c = isHorizontalSpace c || isEndOfLine c

-- | End Of Line Or File
eolof :: Parser ()
{-# INLINE eolof #-}
eolof = endOfLine <|> endOfInput

-- | skips horizontal space and eats EOL or EOF
endOfField :: Parser ()
endOfField = skipHSpace <* eolof

-- | skip horizontal space
skipHSpace :: Parser ()
{-# INLINE skipHSpace #-}
skipHSpace = skipWhile isHorizontalSpace

-- | skip all whitespace (including newlines)
skipVSpace :: Parser ()
{-# INLINE skipVSpace #-}
skipVSpace = skipSpace
