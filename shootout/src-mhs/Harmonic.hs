-- Harmonic.hs - shootout harmonic, fun-version companion (verbatim version:
-- src-verbatim/harmonic.hs).  Same partial harmonic sum at n = 5000, the
-- unboxed-Double# countdown loop expressed on FloatW (binary32, the
-- porting rule); result = sum * 1e6 truncated (the printed value carries
-- 9 decimals; binary32 keeps ~7 digits, the hash takes what the machine
-- has).
module Harmonic where
import Prelude()
import NanoPrelude

loop :: Int -> FloatW -> FloatW
loop d s = if d == 0 then s else loop (d-1) (s +. (fromIntD 1 /. fromIntD d))

bench :: Int
bench = truncateD (loop 5000 (fromIntD 0) *. fromIntD 1000000)

main :: Int
main = bench
