-- RandLcg - the shootout LCG (IM 139968, IA 3877, IC 29573), 20000 draws.
module RandLcg where
import Prelude()
import NanoPrelude
step :: Int -> Int
step s = mod (s * 3877 + 29573) 139968
go :: Int -> Int -> Int
go i s = if i <= 0 then s else go (i-1) (step s)
bench :: Int
bench = go 20000 42
main :: Int
main = bench
