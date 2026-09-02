-- Parts.hs - the Reduceron flite Parts benchmark, ported to the NanoPrelude
-- dialect. Counts the integer partitions of 30; the answer is 5604.
--
-- The port is a transliteration, not a rewrite. Where the F-lite source
-- defines a function that NanoPrelude does not have (concatMap, append,
-- dropWhile) the source's own definition is kept, because the benchmark's
-- work is what is being measured and a library version could fold, fuse or
-- strictify it differently. Where NanoPrelude has an identical one (length,
-- map, min) that is used instead.
module Parts where

import Prelude()
import NanoPrelude

p n = length (partitions n)

partitions n = partitionsWith n (countDown n)

partitionsWith n ns =
  if n == 0
    then [[]]
    else concatMapL (partitionsWith0 n ns) ns

-- the source's own `and`, which is not (&&): it is lazy in its second
-- argument only, and `lt` is built out of it
andL :: Bool -> Bool -> Bool
andL False x = False
andL True  x = x

lt n m = andL (n /= m) (n <= m)

partitionsWith0 n ns i =
  let n0 = n - i
      m  = min i n0
  in map (cons i) (partitionsWith n0 (dropWhileL (lt m) ns))

cons x xs = x : xs

countDown n = if 1 <= n then n : countDown (n - 1) else []

concatMapL f []       = []
concatMapL f (x : xs) = append (f x) (concatMapL f xs)

append []       ys = ys
append (x : xs) ys = x : append xs ys

dropWhileL p xs =
  case xs of
    []      -> []
    (x : ys) -> if p x then dropWhileL p ys else xs

main :: Int
main = p 30
