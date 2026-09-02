-- FloydWn.hs - all-pairs shortest path, n = 12, LCG weights, answer 2918424.
-- Nano dialect; the matrix is one flat array.
module FloydWn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 12

inf :: Int
inf = 100000000

main :: Int
main = newArr (n*n) inf go

go :: IOArray Int -> Int
go d = fill 0 0 1
  where
    fill i j seed
      | i >= n = loopk 0
      | j >= n = fill (i+1) 0 seed
      | otherwise =
          let s = rem (seed*3877 + 29573) 139968
          in if i == j then writeArr d (i*n+j) 0 (fill i (j+1) s)
             else if s < 46656 then writeArr d (i*n+j) (s+1) (fill i (j+1) s)
             else fill i (j+1) s
    loopk k = if k >= n then total 0 0 else loopi k 0
    loopi k i = if i >= n then loopk (k+1) else loopj k i 0
    loopj k i j =
      if j >= n then loopi k (i+1)
      else readArr d (i*n+k) (\a -> readArr d (k*n+j) (\b -> readArr d (i*n+j) (\c ->
             if a + b < c then writeArr d (i*n+j) (a+b) (loopj k i (j+1))
                          else loopj k i (j+1))))
    total i acc =
      if i >= n*n then acc
      else readArr d i (\v -> total (i+1) (if v >= inf then acc else acc + v))
