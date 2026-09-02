{-# LANGUAGE NoImplicitPrelude #-}
module Common (module Common, module GExtra) where
import Prelude
import GExtra
import Data.Word (Word32)

type W = Word32

lcgNext :: W -> W
lcgNext s = s * 1664525 + 1013904223

lcgRange :: W -> W -> (W, W)
lcgRange s n = let s' = lcgNext s
                   v  = s' `umod` (if n == 0 then 1 else n)
               in (v, s')

hashMix :: W -> W -> W
hashMix h v = let a = h `xor` v
                  b = a * 0x85ebca6b
                  c = b `xor` (b `shiftR` 13)
                  d = c * 0xc2b2ae35
              in d `xor` (d `shiftR` 16)

benchFold :: Int -> (Int -> W -> W) -> W
benchFold n step = go 0 0xC0FFEE
  where go i acc | i >= n    = acc
                 | otherwise = go (i + 1) (step i acc)

mul3plus1 :: W -> W
mul3plus1 x = x * 3 + 1
evenW :: W -> Bool
evenW x = (x .&. 1) == 0
addW :: W -> W -> W
addW a b = a + b
xorW :: W -> W -> W
xorW a b = a `xor` b
