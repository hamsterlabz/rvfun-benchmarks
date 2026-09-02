-- SpecNormn.hs - spectral norm, n = 8, ten power iterations, answer 1270046208.
-- Nano dialect. Written monomorphically: a higher-order kernel leaves the
-- float literals' types ambiguous and they desugar through fromRational, which
-- the nano dialect does not have.
module SpecNormn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 8

main :: Int
main = newArr n (1.0::Float) (\uu ->
       newArr n (0.0::Float) (\v ->
       newArr n (0.0::Float) (\t -> loop uu v t 10)))

acell :: Int -> Int -> Float
acell i j = let s = i + j in 1.0 /. fromIntD (quot (s * (s + 1)) 2 + i + 1)

-- y := A x
av :: IOArray Float -> IOArray Float -> Int -> (Int -> Int) -> Int
av x y i k =
  if i >= n then k 0
  else rowa x i 0 0.0 (\a -> writeArr y i a (av x y (i+1) k))

rowa :: IOArray Float -> Int -> Int -> Float -> (Float -> Int) -> Int
rowa x i j acc k =
  if j >= n then k acc
  else readArr x j (\xj -> rowa x i (j+1) (acc +. acell i j *. xj) k)

-- y := A^T x
atv :: IOArray Float -> IOArray Float -> Int -> (Int -> Int) -> Int
atv x y i k =
  if i >= n then k 0
  else colt x i 0 0.0 (\a -> writeArr y i a (atv x y (i+1) k))

colt :: IOArray Float -> Int -> Int -> Float -> (Float -> Int) -> Int
colt x i j acc k =
  if j >= n then k acc
  else readArr x j (\xj -> colt x i (j+1) (acc +. acell j i *. xj) k)

dot :: IOArray Float -> IOArray Float -> Int -> Float -> (Float -> Int) -> Int
dot x y i acc k =
  if i >= n then k acc
  else readArr x i (\a -> readArr y i (\b -> dot x y (i+1) (acc +. a *. b) k))

loop :: IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int
loop uu v t c =
  if c <= 0
    then dot uu v 0 0.0 (\vbv -> dot v v 0 0.0 (\vv ->
           truncateD (sqrtD (vbv /. vv) *. 1000000000.0)))
    else av uu t 0 (\_ -> atv t v 0 (\_ ->
         av v t 0 (\_ -> atv t uu 0 (\_ -> loop uu v t (c-1)))))
