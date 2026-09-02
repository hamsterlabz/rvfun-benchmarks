-- Allocn.hs - the allocation stress test: 20 x 1000 three-element arrays,
-- answer 120000. Nano dialect; each object is a real heap array.
module Allocn where
import Prelude()
import NanoPrelude
import NanoArray

main :: Int
main = loop 20 0

create :: Int -> Int -> Int
create k acc =
  if k <= 0 then acc
  else newArr 3 (0::Int) (\o ->
         writeArr o 0 5 (writeArr o 1 k (writeArr o 2 1 (
           readArr o 0 (\a -> readArr o 2 (\b -> create (k-1) (acc + a + b)))))))

loop :: Int -> Int -> Int
loop i acc = if i <= 0 then acc else loop (i-1) (acc + create 1000 0)
