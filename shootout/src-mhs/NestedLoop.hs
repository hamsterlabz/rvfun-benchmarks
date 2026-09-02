-- NestedLoop - shootout nestedloop, n = 6 -> 6^6 = 46656.
module NestedLoop where
import Prelude()
import NanoPrelude
nn :: Int
nn = 6
lp :: Int -> Int -> Int -> Int
lp d i acc =
  if i >= nn then acc
  else lp d (i+1) (if d <= 1 then acc + 1 else lp (d-1) 0 acc)
bench :: Int
bench = lp 6 0 0
main :: Int
main = bench
