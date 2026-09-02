-- MatrixM - shootout matrix: 20x20 integer multiply, 3 passes, reporting
-- mm[0][0] + mm[2][3] + mm[3][2] + mm[4][4].  Row-major in a flat array.
module MatrixM where
import Prelude()
import NanoPrelude
import NanoArray
sz :: Int
sz = 20
n2 :: Int
n2 = 400
bench :: Int
bench = newArr n2 0 (\m1 -> newArr n2 0 (\m2 -> newArr n2 0 (\mm ->
          initf 0 1 m1 m2 (pass 3 m1 m2 mm (
            readArr mm 0 (\a ->
            readArr mm (2*sz+3) (\b ->
            readArr mm (3*sz+2) (\c ->
            readArr mm (4*sz+4) (\d -> a + b + c + d)))))))))
initf :: Int -> Int -> IOArray Int -> IOArray Int -> a -> a
initf i c m1 m2 k =
  if i >= n2 then k
  else writeArr m1 i c (writeArr m2 i c (initf (i+1) (c+1) m1 m2 k))
dot :: Int -> Int -> Int -> Int -> IOArray Int -> IOArray Int -> (Int -> a) -> a
dot i j kk acc m1 m2 k =
  if kk >= sz then k acc
  else readArr m1 (i*sz+kk) (\u -> readArr m2 (kk*sz+j) (\v ->
         dot i j (kk+1) (acc + u*v) m1 m2 k))
cols :: Int -> Int -> IOArray Int -> IOArray Int -> IOArray Int -> a -> a
cols i j m1 m2 mm k =
  if j >= sz then k
  else dot i j 0 0 m1 m2 (\s -> writeArr mm (i*sz+j) s (cols i (j+1) m1 m2 mm k))
rows :: Int -> IOArray Int -> IOArray Int -> IOArray Int -> a -> a
rows i m1 m2 mm k = if i >= sz then k else cols i 0 m1 m2 mm (rows (i+1) m1 m2 mm k)
pass :: Int -> IOArray Int -> IOArray Int -> IOArray Int -> a -> a
pass n m1 m2 mm k = if n <= 0 then k else rows 0 m1 m2 mm (pass (n-1) m1 m2 mm k)
main :: Int
main = bench
