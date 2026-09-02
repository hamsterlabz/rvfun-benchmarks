-- Strcat.hs - shootout strcat, fun-version companion (verbatim version:
-- src-verbatim/strcat.hs).  The Haskell entry IS
--   length (concat (replicate n "hello\n"))
-- and that expression is computable directly in the nano dialect; n = 2000
-- as the other versions.
module Strcat where
import Prelude()
import NanoPrelude
import Primitives (Char)

hello :: [Char]
hello = "hello\n"

replicateL :: Int -> [Char] -> [[Char]]
replicateL k x = if k <= 0 then [] else x : replicateL (k-1) x

concatL :: [[Char]] -> [Char]
concatL []       = []
concatL (x:xs)   = app x (concatL xs)
  where app []     ys = ys
        app (c:cs) ys = c : app cs ys

bench :: Int
bench = length (concatL (replicateL 2000 hello))

main :: Int
main = bench
