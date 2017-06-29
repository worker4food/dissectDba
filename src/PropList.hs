module PropList where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

-- | Skip all space around specific char
skipSA c = skipSpace *> char c <* skipSpace

quote = char '"'

escEol = (char '\n' <|> char '\r') <* skipSpace <* char '|'

escQuote = quote *> try quote

-- | Useless, since escaped eol's not supported
legalChar = escEol <|> escQuote <|> satisfy (notInClass "\r\n\"")

str = quote *> many' (escQuote <|> notChar '"') <* quote

kv = do
  _ <- skipSA '{' <?> "pair open"
  key <- str      <?> "key part"
  _ <- skipSA ',' <?> "after key part"
  val <- str      <?> "value part"
  _ <- skipSA '}' <?> "pair close"
  pure (key, val)

propList = do
  _ <- skipSA '{'             <?> "list open"
  r <- kv `sepBy1` skipSA ',' <?> "list values"
  _ <- skipSA '}'             <?> "list close"
  pure r
