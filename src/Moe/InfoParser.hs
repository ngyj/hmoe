{-# language LambdaCase #-}
module Moe.InfoParser where

import Debug.Trace (trace, traceShowId)
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
           | Wp Bool
           deriving (Eq, Show)

parseLines :: [Text] -> [Img]
parseLines = foldImg
             . filter (/= Left "Failed reading: empty")
             . map (parseOnly ptypeP)

-- REFACTOR Lenses and folds, please
-- TODO log when overwriting field, report line numbers?
foldImg :: [Either String PType] -> [Img]
foldImg = \case
  [] -> []
  (Right (Fn s):xs) -> go (defaultImg s) xs
  _ -> error "image_info format error: should start with a filename"
  where
    defaultImg s = Img s Nothing Nothing [] False
    go acc [] = acc `seq` [acc]
    go _ (Left _:_) = trace "TODO logError" []
    go acc (Right x:xs) = case x of
      Fn s -> acc : go (defaultImg s) xs
      Cat s -> go acc{imCat=Just s} xs
      Src s -> go acc{imSrc=Just s} xs
      Tag s -> go acc{imTag=s} xs
      Wp s -> go acc{imWp=s} xs

ptypeP :: Parser PType
ptypeP = choice [fnP, srcP, catP, tagP, wpP]

-- PERFORMANCE profile and determine wheter @. trim <$>@ is faster than @skipSpace@ everyhwere
fnP :: Parser PType
fnP = Fn <$>
  (skipSpace *> char '[' *> skipSpace
  *> takeWhile (not . isHorizontalSpace <&&> (/=']'))
  <* skipSpace <* char ']')

srcP :: Parser PType
srcP = kvPairP (string "source" <|> string "src") $
       Src <$> takeWhile (not . isHorizontalSpace)


catP :: Parser PType
catP = kvPairP (string "category" <|> string "cat") $
       Cat <$> takeWhile (not . isHorizontalSpace)

tagP :: Parser PType
tagP = kvPairP (string "tags") $
       Tag . splitTags <$> takeText
  where
    splitTags = filter (not . T.null) . map strip . splitOn "," . strip . traceShowId

wpP :: Parser PType
wpP = kvPairP (string "wp" <|> string "wallpaper") $
      Wp <$> boolP

boolP :: Parser Bool
boolP = (asciiCI "true" *> pure True)
        <|> (asciiCI "false" *> pure False)

kvPairP :: Parser Text -> Parser PType -> Parser PType
kvPairP keyP valueP = skipSpace
  *> keyP
  *> skipSpace *> char ':'
  *> skipSpace
  *> valueP
