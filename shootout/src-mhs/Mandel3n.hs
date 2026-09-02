-- Mandel3n.hs - mirrors benches/mandelbrot.ml of min-caml-hs: the same escape
-- test over the same n x n grid on [-1.5,0.5]x[-1,1], fifty iterations,
-- reporting the population count of the bitmap.
-- NanoPrelude dialect: this backend is FullPrelude-free and main is a pure Int
-- that the report epilogue prints. SIM SCALE: n = 12, matching the scaled
-- min-caml run it is compared against (60 of 144 inside).
module Mandel3n where
import Prelude()
import NanoPrelude

n :: Int
n = 12

inside :: Float -> Float -> Float -> Float -> Int -> Int
inside cr ci zr zi k =
  if k >= 50 then 1
  else if (zr *. zr +. zi *. zi) >. 4.0 then 0
  else inside cr ci (zr *. zr -. zi *. zi +. cr) (2.0 *. zr *. zi +. ci) (k+1)

row :: Int -> Int -> Int -> Int
row i j acc =
  if j >= n then acc
  else row i (j+1) (acc + inside (2.0 *. fromIntD j /. fromIntD n -. 1.5)
                                 (2.0 *. fromIntD i /. fromIntD n -. 1.0)
                                 0.0 0.0 0)

grid :: Int -> Int -> Int
grid i acc = if i >= n then acc else grid (i+1) (acc + row i 0 0)

main :: Int
main = grid 0 0
