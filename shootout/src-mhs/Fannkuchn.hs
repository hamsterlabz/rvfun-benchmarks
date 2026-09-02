-- Fannkuchn.hs - fannkuch-redux at n = 6: checksum 49, max flips 10, printed
-- concatenated as 4910 (as the OCaml does). Nano dialect.
module Fannkuchn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 6

main :: Int
main = newArr n (0::Int) (\perm ->
       newArr n (0::Int) (\perm1 ->
       newArr n (0::Int) (\cnt -> setup perm perm1 cnt 0)))

setup :: IOArray Int -> IOArray Int -> IOArray Int -> Int -> Int
setup perm perm1 cnt i =
  if i >= n then loop perm perm1 cnt 0 0 0
  else writeArr perm1 i i (writeArr cnt i (i+1) (setup perm perm1 cnt (i+1)))

copyperm :: IOArray Int -> IOArray Int -> Int -> Int -> Int
copyperm perm perm1 i k =
  if i >= n then k
  else readArr perm1 i (\v -> writeArr perm i v (copyperm perm perm1 (i+1) k))

rev :: IOArray Int -> Int -> Int -> Int -> Int
rev perm i j k =
  if i >= j then k
  else readArr perm i (\a -> readArr perm j (\b ->
         writeArr perm i b (writeArr perm j a (rev perm (i+1) (j-1) k))))

flips :: IOArray Int -> Int -> (Int -> Int) -> Int
flips perm f k =
  readArr perm 0 (\p0 -> if p0 == 0 then k f else rev perm 0 p0 (flips perm (f+1) k))

rotate :: IOArray Int -> Int -> Int -> Int -> Int
rotate perm1 i r k =
  if i >= r then k
  else readArr perm1 (i+1) (\v -> writeArr perm1 i v (rotate perm1 (i+1) r k))

nextperm :: IOArray Int -> IOArray Int -> Int -> (Int -> Int) -> Int
nextperm perm1 cnt r k =
  if r >= n then k 0
  else readArr perm1 0 (\p0 ->
         rotate perm1 0 r (
           writeArr perm1 r p0 (
             readArr cnt r (\c ->
               writeArr cnt r (c-1) (
                 if c - 1 > 0 then k 1
                 else writeArr cnt r (r+1) (nextperm perm1 cnt (r+1) k))))))

loop :: IOArray Int -> IOArray Int -> IOArray Int -> Int -> Int -> Int -> Int
loop perm perm1 cnt ev mx cs =
  copyperm perm perm1 0 (
    flips perm 0 (\f ->
      let mx2 = if f > mx then f else mx
          cs2 = if ev == 0 then cs + f else cs - f
      in seq mx2 (seq cs2 (nextperm perm1 cnt 1 (\nx ->
           if nx == 1 then loop perm perm1 cnt (1-ev) mx2 cs2
                      else cs2 * 100 + mx2)))))
