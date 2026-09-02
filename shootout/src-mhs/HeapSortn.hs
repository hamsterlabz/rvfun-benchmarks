-- HeapSortn.hs - mirrors benches/heapsort.ml: LCG fill, sift-down heapsort,
-- weighted checksum. NanoPrelude dialect, arrays via NanoArray's CPS atoms.
-- SIM SCALE: n = 400, matching the scaled min-caml run (396240226).
module HeapSortn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 100

main :: Int
main = newArr n (0::Int) go

go :: IOArray Int -> Int
go a = fill 0 1
  where
    fill i seed =
      if i >= n then build (n `quot` 2 - 1)
      else let s = (seed * 3877 + 29573) `rem` 139968
           in writeArr a i s (fill (i+1) s)

    sift root last =
      let c = 2 * root + 1
      in if c > last then 0
         else readArr a c (\vc ->
                pick c last vc (\c2 ->
                  readArr a c2 (\vc2 -> readArr a root (\vr ->
                    if vc2 > vr
                      then writeArr a root vc2 (writeArr a c2 vr (sift c2 last))
                      else 0))))

    pick c last vc k =
      if c + 1 <= last
        then readArr a (c+1) (\vn -> if vn > vc then k (c+1) else k c)
        else k c

    build i = if i < 0 then drain (n-1) else seq (sift i (n-1)) (build (i-1))

    drain e =
      if e <= 0 then sum 0 0
      else readArr a 0 (\v0 -> readArr a e (\ve ->
             writeArr a 0 ve (writeArr a e v0 (seq (sift 0 (e-1)) (drain (e-1))))))

    sum i acc =
      if i >= n then acc
      else readArr a i (\v -> sum (i+1) ((acc + v * (i+1)) `rem` 1000000007))
