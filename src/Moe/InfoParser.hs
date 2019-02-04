{-# language LambdaCase #-}
module Moe.InfoParser where

import Debug.Trace (trace)
import Prelude hiding (takeWhile)

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.Text
import Data.Text as T (Text, strip, splitOn, null)

import Moe.Img

infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)

data PType = Fn Text
           | Cat Text
           | Src Text
           | Tag [Text]
           deriving (Eq, Show)


-- REFACTOR
-- TODO log when overwriting field, report line numbers?
foldImg :: [Either String PType] -> [Img]
foldImg = \case
  [] -> []
  (Right (Fn s):xs) -> go (Img s Nothing Nothing []) xs
  _ -> error "image_info format error: should start with a filename"
  where
    go acc [] = acc `seq` [acc]
    go acc (Left x:xs) = trace "TODO logError" []
    go acc (Right x:xs) = case x of
      Fn s -> acc : go (Img s Nothing Nothing []) xs
      Cat s -> go acc{imCat=Just s} xs
      Src s -> go acc{imSrc=Just s} xs
      Tag s -> go acc{imTag=s} xs

parseLines :: [Text] -> [Either String PType]
parseLines = filter (/= Left "Failed reading: empty") . map (parseOnly ptypeP)

ptypeP :: Parser PType
ptypeP = choice [fnP, srcP, catP, tagP]

fnP :: Parser PType
fnP = Fn <$>
  (skipSpace *> char '[' *> skipSpace
  *> takeWhile (not . isHorizontalSpace <&&> (/=']'))
  <* skipSpace <* char ']')

srcP :: Parser PType
srcP = Src <$>
  kvPairP (string "source" <|> string "src")

catP :: Parser PType
catP = Cat <$>
  kvPairP (string "category" <|> string "cat")

tagP :: Parser PType
tagP = Tag . splitTags <$>
  kvPairP (string "tags")
  where
    splitTags = filter (not . T.null) . map strip . splitOn "," . strip

kvPairP :: Parser Text -> Parser Text
kvPairP keyP = skipSpace
  *> keyP
  *> skipSpace *> char ':'
  *> skipSpace
  *> takeWhile (not . isHorizontalSpace)
