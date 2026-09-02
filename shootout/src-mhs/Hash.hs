-- Hash.hs - shootout hash, fun-version companion (verbatim version:
-- src-verbatim/hash.hs, n = 100).  The same table: insert keys showHex 1..n
-- with value True, then count hits looking up showInt n..1; the finite map
-- is an association list (n = 100 keys).  Hex/dec renderers are the glue
-- the dialect lacks.
module Hash where
import Prelude()
import NanoPrelude
import Primitives (Char, primChr, primOrd)

digit :: Int -> Char
digit d = if d < 10 then primChr (48 + d) else primChr (87 + d)  -- 0-9,a-f

showBase :: Int -> Int -> [Char]
showBase b v = go v []
  where go x acc = let q = quot x b
                       r = rem x b
                   in if q == 0 then digit r : acc else go q (digit r : acc)

eqS :: [Char] -> [Char] -> Bool
eqS []     []     = True
eqS (a:as) (b:bs) = primOrd a == primOrd b && eqS as bs
eqS _      _      = False

lookupS :: [([Char], Bool)] -> [Char] -> Bool
lookupS []          _ = False
lookupS ((k,v):kvs) q = if eqS k q then v else lookupS kvs q

n :: Int
n = 100

bench :: Int
bench = go n 0
  where
    tbl = build 1
    build i = if i > n then [] else (showBase 16 i, True) : build (i+1)
    go 0 c = c
    go i c = go (i-1) (if lookupS tbl (showBase 10 i) then c+1 else c)

main :: Int
main = bench
