-- Crc32n.hs - standard reflected CRC-32 over an LCG byte stream, n = 2000,
-- answer 689943297 (matches python zlib.crc32). Nano dialect, bitwise via
-- NanoBits.
module Crc32n where
import Prelude()
import NanoPrelude
import NanoArray
import NanoBits

n :: Int
n = 2000

mask :: Int
mask = 0 - 1          -- all ones in 32 bits

main :: Int
main = newArr 256 (0::Int) go

go :: IOArray Int -> Int
go tbl = mktbl 0
  where
    ent c k = if k >= 8 then c
              else if andI c 1 == 1 then ent (xorI 3988292384 (shrI c 1)) (k+1)
                                    else ent (shrI c 1) (k+1)
    mktbl i = if i >= 256 then crc 0 1 mask
              else writeArr tbl i (ent i 0) (mktbl (i+1))
    crc i seed c =
      if i >= n then xorI c mask
      else let s = rem (seed*3877 + 29573) 139968
               b = andI s 255
           in readArr tbl (andI (xorI c b) 255) (\t ->
                crc (i+1) s (xorI t (andI (shrI c 8) 16777215)))
