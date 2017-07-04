{-# LANGUAGE OverloadedStrings #-}
module PropList (propList) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

-- | Skip all space around specific parser
skipSA p = skipSpace *> p *> skipSpace

quote = char '"'

escQuote = quote *> try quote

str = quote *> many' (escQuote <|> notChar '"') <* quote

kv = do
  skipSA "{" <?> "pair open"
  key <- str <?> "key part"
  skipSA "," <?> "after key part"
  val <- str <?> "value part"
  skipSA "}" <?> "pair close"
  pure (key, val)

propList :: Parser [(String, String)]
propList = do
  skipSA "{"                  <?> "list open"
  r <- kv `sepBy1` skipSA "," <?> "list values"
  skipSA "}"                  <?> "list close"
  pure r
