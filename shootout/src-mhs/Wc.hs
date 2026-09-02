-- Wc.hs - shootout wc, fun-version companion (verbatim version: src-verbatim/wc.hs,
-- same input embedded as WcInput.text since the fun backend has no stdin).
-- The same three counts with the same word state machine (prevIsSpace
-- transition), folded with the suite hash: h = ((nl*31)+nw)*31+nc.
module Wc where
import Prelude()
import NanoPrelude
import Primitives (Char, primOrd)
import WcInput

isspace :: Char -> Bool
isspace c = primOrd c == 32 || primOrd c == 10 || primOrd c == 9

isnl :: Char -> Bool
isnl c = primOrd c == 10

wcLoop :: Int -> Int -> Int -> Int -> [Char] -> (Int, (Int, Int))
wcLoop prevIsSpace nl nw nc []     = (nl, (nw, nc))
wcLoop prevIsSpace nl nw nc (c:cs) =
  let cIsSpace = if isspace c then 1 else 0
      nl' = if isnl c then nl + 1 else nl
      nw' = if prevIsSpace > cIsSpace then nw + 1 else nw
  in wcLoop cIsSpace nl' nw' (nc + 1) cs

bench :: Int
bench = (nl*31 + nw)*31 + nc
  where (nl, (nw, nc)) = wcLoop 1 0 0 0 text

main :: Int
main = bench
