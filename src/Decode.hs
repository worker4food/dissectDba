module Decode where

import           Data.Bits
import           Numeric                         (showHex)

import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.Lazy.Char8      as C


key = C.pack $ cycle "19465912879oiuxc ensdfaiuo3i73798kjl"

encode = B.pack . B.zipWith xor key
decode = encode

mkChecksum = fmap (`showHex` "") . eitherResult . parsed
  where
    parsed = parse (sum <$> many1 anyWord32le <* endOfInput <?> "Broken userdef.usr")
