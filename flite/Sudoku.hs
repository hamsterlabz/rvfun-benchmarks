-- Sudoku.hs - the Reduceron flite Sudoku benchmark (Bird's solver), ported to
-- the NanoPrelude dialect. Transliteration, not a rewrite: every function the
-- F-lite source defines is defined here too, with an L suffix where the name
-- would clash with NanoPrelude. The benchmark's work is its own definitions,
-- so a library version that folds or fuses differently would not be the same
-- program.
--
-- The source's `main` emits the solved board over a character stream. This
-- version has no output channel, so consumption becomes a structural hash of the
-- board, exactly as the Treejoin port does. The hash forces all 81 cells.
module Sudoku where

import Prelude()
import NanoPrelude

delL x []       = []
delL x (y : ys) = if x == y then ys else y : delL x ys

diffL xs []       = xs
diffL xs (y : ys) = diffL (delL y xs) ys

headL (x : xs) = x
tailL (x : xs) = xs

lengthL []       = 0
lengthL (x : xs) = 1 + lengthL xs

nullL []      = True
nullL (x : xs) = False

singleL []       = False
singleL (x : xs) = nullL xs

minimumL (x : xs) = minL x xs

minL m []       = m
minL m (x : xs) = if x <= m then minL x xs else minL m xs

breakL p []       = ([], [])
breakL p (x : xs) =
  if p x
    then ([], x : xs)
    else case breakL p xs of (ys, zs) -> (x : ys, zs)

filterL p []       = []
filterL p (x : xs) = if p x then x : filterL p xs else filterL p xs

zipWithL f []       ys       = []
zipWithL f (x : xs) []       = []
zipWithL f (x : xs) (y : ys) = f x y : zipWithL f xs ys

notElemL x []       = True
notElemL x (y : ys) = andL (x /= y) (notElemL x ys)

-- the source's own and/or: lazy in the second argument, not (&&)/(||)
andL False x = False
andL True  x = x

orL False x = x
orL True  x = True

notL False = True
notL True  = False

anyL p []       = False
anyL p (x : xs) = orL (p x) (anyL p xs)

allL p []       = True
allL p (x : xs) = andL (p x) (allL p xs)

mapL f []       = []
mapL f (x : xs) = f x : mapL f xs

appendL []       ys = ys
appendL (x : xs) ys = x : appendL xs ys

concatL []         = []
concatL (xs : xss) = appendL xs (concatL xss)

concatMapL f []       = []
concatMapL f (x : xs) = appendL (f x) (concatMapL f xs)

takeL n []       = []
takeL n (x : xs) = if n == 0 then [] else x : takeL (n - 1) xs

dropL n []       = []
dropL n (x : xs) = if n == 0 then x : xs else dropL (n - 1) xs

groupByL n xs = if nullL xs then [] else takeL n xs : groupByL n (dropL n xs)

idL x = x

compL f g x = f (g x)

boardsize = 9
boxsize   = 3
cellvals  = [1, 2, 3, 4, 5, 6, 7, 8, 9]

blank x = x == 0

nodups []       = True
nodups (x : xs) = andL (notElemL x xs) (nodups xs)

singletonL x = [x]

cols [xs]            = mapL singletonL xs
cols (xs : ys : yss) = zipWithL cons xs (cols (ys : yss))

cons x xs = x : xs

boxs m = mapL concatL (concatMapL cols (groupByL 3 (mapL (groupByL 3) m)))

choices b = mapL (mapL choose) b

choose e = if blank e then cellvals else [e]

fixed css = concatL (filterL singleL css)

reduce css = mapL (remove (fixed css)) css

remove fs cs = if singleL cs then cs else diffL cs fs

prune m = pruneBy boxs (pruneBy cols (pruneBy idL m))

pruneBy f m = f (mapL reduce (f m))

blocked cm = orL (void cm) (notL (safe cm))

void m = anyL (anyL nullL) m

safe cm = andL (allL (compL nodups fixed) cm)
               (andL (allL (compL nodups fixed) (cols cm))
                     (allL (compL nodups fixed) (boxs cm)))

best n cs = lengthL cs == n

expand cm =
  let n = minchoice cm
  in case breakL (anyL (best n)) cm of
       (rows1, rows2) ->
         case breakL (best n) (headL rows2) of
           (row1, row2) -> mapL (expOne row1 row2 rows1 rows2) (headL row2)

expOne row1 row2 rows1 rows2 c =
  appendL rows1 (appendL [appendL row1 ([c] : tailL row2)] (tailL rows2))

minchoice m = minimumL (filterL gte2 (concatMapL (mapL lengthL) m))

gte2 x = 2 <= x

search cm =
  if blocked cm
    then []
    else if allL (allL singleL) cm
           then [cm]
           else concatMapL (compL search prune) (expand cm)

sudoku b = mapL (mapL (mapL headL)) (search (prune (choices b)))

-- structural hash of the solved board, in place of the source's emit
hashRow acc []       = acc
hashRow acc (x : xs) = hashRow (acc * 31 + x) xs

hashBoard acc []       = acc
hashBoard acc (r : rs) = hashBoard (hashRow acc r) rs

-- SIM SCALE: the upstream grid has 17 givens, which is a deliberately hard
-- search and ran past 17M cycles on the RTL without finishing.  This is the
-- same solver on a 30-given grid.  BOTH ARMS use this grid, so the fun-vs-GHC
-- comparison is unaffected; only the absolute answer differs from upstream.
puzzle =
  [ [5,3,0, 0,7,0, 0,0,0]
  , [6,0,0, 1,9,5, 0,0,0]
  , [0,9,8, 0,0,0, 0,6,0]
  , [8,0,0, 0,6,0, 0,0,3]
  , [4,0,0, 8,0,3, 0,0,1]
  , [7,0,0, 0,2,0, 0,0,6]
  , [0,6,0, 0,0,0, 2,8,0]
  , [0,0,0, 4,1,9, 0,0,5]
  , [0,0,0, 0,8,0, 0,7,9]
  ]

main :: Int
main = hashBoard 0 (headL (sudoku puzzle))
