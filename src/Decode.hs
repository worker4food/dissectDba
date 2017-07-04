module Decode where

import           Data.Bits                  (xor)
import           Numeric                    (showHex)

import           Data.Binary.Get
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C

key = C.pack $ cycle "19465912879oiuxc ensdfaiuo3i73798kjl"

encode = B.pack . B.zipWith xor key
decode = encode

mkChecksum :: ByteString -> Either String String
mkChecksum = extract . runGetOrFail (sumOf 0)
  where
    extract (Left  _)         = Left "Broken userdef.usr"
    extract (Right (_, _, x)) = Right $ showHex x ""

    sumOf acc = isEmpty >>= \eof ->
        if eof
            then pure acc
            else do
              x <- (acc + ) <$> getWord32le
              x `seq` sumOf x
