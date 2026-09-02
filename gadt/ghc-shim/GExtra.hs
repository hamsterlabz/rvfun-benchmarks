{-# LANGUAGE NoImplicitPrelude #-}
module GExtra (module GExtra, module Prelude) where
import Prelude
import Data.Word (Word32)
import qualified Data.Bits as B

infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.
infixl 8 `shiftL`, `shiftR`

xor :: Word32 -> Word32 -> Word32
xor = B.xor
(.&.) :: Word32 -> Word32 -> Word32
(.&.) = (B..&.)
(.|.) :: Word32 -> Word32 -> Word32
(.|.) = (B..|.)
shiftL :: Word32 -> Int -> Word32
shiftL x n = B.shiftL x n
shiftR :: Word32 -> Int -> Word32        -- LOGICAL (Word32 is unsigned)
shiftR x n = B.shiftR x n
umod :: Word32 -> Word32 -> Word32        -- unsigned (Word32 mod)
umod = mod
udiv :: Word32 -> Word32 -> Word32
udiv = div

-- W -> Float, unsigned; op-for-op the mhs GExtra.wToD sequence.
wToD :: Word32 -> Float
wToD w = fromIntegral (shiftR w 16) * 65536 + fromIntegral (w .&. 65535)
