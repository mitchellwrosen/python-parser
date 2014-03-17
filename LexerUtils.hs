module LexerUtils where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Data.Word (Word8)

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
    go :: Int -> [Int]
    go oc
        | oc <= 0x7f   = [oc]
        | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
                         , 0x80 + oc .&. 0x3f
                         ]
        | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
                         , 0x80 + ((oc `shiftR` 6) Data.Bits..&. 0x3f)
                         , 0x80 + oc .&. 0x3f
                         ]
        | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                         , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                         , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                         , 0x80 + oc .&. 0x3f
                         ]
