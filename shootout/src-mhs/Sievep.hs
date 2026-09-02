-- Sievep - shootout sieve: Eratosthenes to 4096, 3 passes, prime count.
module Sievep where
import Prelude()
import NanoPrelude
import NanoArray
lim :: Int
lim = 4096
bench :: Int
bench = newArr (lim+1) 1 (\f -> pass 3 f 0)
pass :: Int -> IOArray Int -> Int -> Int
pass n f acc =
  if n <= 0 then acc
  else fill 2 f (sweep 2 f 0 (\c -> pass (n-1) f c))
fill :: Int -> IOArray Int -> a -> a
fill i f k = if i > lim then k else writeArr f i 1 (fill (i+1) f k)
sweep :: Int -> IOArray Int -> Int -> (Int -> a) -> a
sweep i f c k =
  if i > lim then k c
  else readArr f i (\v ->
         if v == 1 then clear (i+i) i f (sweep (i+1) f (c+1) k)
         else sweep (i+1) f c k)
clear :: Int -> Int -> IOArray Int -> a -> a
clear j i f k = if j > lim then k else writeArr f j 0 (clear (j+i) i f k)
main :: Int
main = bench
