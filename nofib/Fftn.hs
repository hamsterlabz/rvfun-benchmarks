-- Fftn.hs - radix-2 decimation-in-time FFT, n = 32, checksum/1000, answer 0.
-- Nano dialect. This is the benchmark that could not be ported until sinD/cosD
-- existed on the fun backend.
module Fftn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 32

half :: Int
half = 16

main :: Int
main = newArr n (0.0::Float) (\yre ->
       newArr n (0.0::Float) (\yim ->
       newArr half (0.0::Float) (\wre ->
       newArr half (0.0::Float) (\wim -> mkw yre yim wre wim 0))))

bitrev :: Int -> Int -> Int -> Int
bitrev acc nb i = if nb == 0 then acc else bitrev (acc*2 + rem i 2) (nb-1) (quot i 2)

mkw :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int
mkw yre yim wre wim k =
  if k >= half then perm yre yim wre wim 0
  else let a = (0.0 -. 6.28318530717958) *. fromIntD k /. fromIntD n
       in writeArr wre k (cosD a) (writeArr wim k (sinD a) (mkw yre yim wre wim (k+1)))

perm :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int
perm yre yim wre wim i =
  if i >= n then pass yre yim wre wim 2
  else writeArr yre i (sinD (fromIntD (bitrev 0 5 i)))
         (writeArr yim i 0.0 (perm yre yim wre wim (i+1)))

pass :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int
pass yre yim wre wim m =
  if m > n then chk yre yim 0 0.0
  else block yre yim wre wim m (quot m 2) (quot n m) 0

block :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
      -> Int -> Int -> Int -> Int -> Int
block yre yim wre wim m hm step ofs =
  if ofs >= n then pass yre yim wre wim (m*2)
  else bfly yre yim wre wim m hm step ofs 0

bfly :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
     -> Int -> Int -> Int -> Int -> Int -> Int
bfly yre yim wre wim m hm step ofs i =
  if i >= hm then block yre yim wre wim m hm step (ofs+m)
  else let j = ofs + i
           k = j + hm
           tw = i * step
       in readArr yre k (\br -> readArr yim k (\bi ->
          readArr wre tw (\cr -> readArr wim tw (\ci ->
          readArr yre j (\ar -> readArr yim j (\ai ->
            let pr = br *. cr -. bi *. ci
                pi2 = br *. ci +. bi *. cr
            in writeArr yre j (ar +. pr) (writeArr yim j (ai +. pi2)
               (writeArr yre k (ar -. pr) (writeArr yim k (ai -. pi2)
                 (bfly yre yim wre wim m hm step ofs (i+1)))))))))))

chk :: IOArray Float -> IOArray Float -> Int -> Float -> Int
chk yre yim i acc =
  if i >= n then truncateD (acc /. 1000.0)
  else readArr yre i (\a -> readArr yim i (\b -> chk yre yim (i+1) (acc +. a *. a +. b *. b)))
