-- BinTrees.hs - the Benchmarks Game binary-trees kernel
-- (shootout/bench/binarytrees/binarytrees.ghc, Don Stewart's entry), same
-- structure, nano dialect.  N is fixed rather than read from argv and the
-- three printf lines are folded into the single checksum the C and OCaml versions
-- also report, so all the compilers are compared by one number.
-- shiftL is written out because the nano dialect has no Data.Bits.
module BinTrees where
import Prelude()
import NanoPrelude

data Tree = Nil | Node Int Tree Tree

minN :: Int
minN = 4

maxN :: Int
maxN = 8

make :: Int -> Int -> Tree
make i d = if d <= 0 then Node i Nil Nil
           else Node i (make (2*i-1) (d-1)) (make (2*i) (d-1))

check :: Tree -> Int
check Nil = 0
check (Node i l r) = i + check l - check r

-- 1 `shiftL` k
shl :: Int -> Int -> Int
shl b k = if k <= 0 then b else shl (b+b) (k-1)

sumT :: Int -> Int -> Int -> Int
sumT d i t = if i <= 0 then t
             else sumT d (i-1) (t + check (make i d) + check (make (0-i) d))

depths :: Int -> Int
depths d = if d > maxN then 0
           else sumT d (shl 1 (maxN - d + minN)) 0 + depths (d+2)

bench :: Int
bench = check (make 0 (maxN+1)) + depths minN + check (make 0 maxN)

main :: Int
main = bench
