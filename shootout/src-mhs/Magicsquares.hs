-- Magicsquares.hs - shootout magicsquares, fun-version companion (verbatim
-- version: src-verbatim/magicsquares.hs, n = 3).  The same best-first search:
-- grid is a 9-cell row-major list (the dialect has no UArray), the
-- priority queue an insertion-sorted list ordered by (priority, grid) as
-- upstream's Ord Square; findFewestMoves / possibleMoves are ported
-- clause for clause.  bench folds the first solution's cells.
module Magicsquares where
import Prelude()
import NanoPrelude

n, mn :: Int
n = 3
mn = 15                        -- n(n^2+1)/2

at :: [Int] -> Int -> Int -> Int
at g r c = idx g ((r-1)*n + (c-1))
  where idx (x:_)  0 = x
        idx (_:xs) k = idx xs (k-1)
        idx []     _ = 0

setAt :: [Int] -> Int -> Int -> Int -> [Int]
setAt g r c v = go g ((r-1)*n + (c-1))
  where go (_:xs) 0 = v : xs
        go (x:xs) k = x : go xs (k-1)
        go []     _ = []

gridRow, gridCol :: [Int] -> Int -> [Int]
gridRow g r = [at g r i | i <- range 1 n]
gridCol g c = [at g i c | i <- range 1 n]
diag1, diag2 :: [Int] -> [Int]
diag1 g = [at g i i | i <- range 1 n]
diag2 g = [at g i (n+1-i) | i <- range 1 n]

reverseL :: [Int] -> [Int]
reverseL = foldl (\acc x -> x : acc) []

range :: Int -> Int -> [Int]
range a b = if a > b then [] else a : range (a+1) b

count0s :: [Int] -> Int
count0s xs = length [x | x <- xs, x == 0]

scanlS :: Int -> [Int] -> [Int]
scanlS a []     = [a]
scanlS a (x:xs) = a : scanlS (a+x) xs

nth :: [Int] -> Int -> Int
nth (x:_)  0 = x
nth (_:xs) k = nth xs (k-1)
nth []     _ = 0

takeW, dropW :: Int -> [Int] -> [Int]      -- takeWhile (<= m) / dropWhile (< m)
takeW m (x:xs) = if x <= m then x : takeW m xs else []
takeW _ []     = []
dropW m l@(x:xs) = if x < m then dropW m xs else l
dropW _ []       = []

possibleMoves :: [Int] -> [Int] -> Int -> Int -> [Int]
possibleMoves g unus r c =
  if at g r c /= 0 then []
  else takeW ma (dropW mi unus)
  where
    cellGroups =
      if r == c && r + c == n + 1 then [diag1 g, diag2 g, gridRow g r, gridCol g c]
      else if r == c              then [diag1 g, gridRow g r, gridCol g c]
      else if r + c == n + 1      then [diag2 g, gridRow g r, gridCol g c]
      else                             [gridRow g r, gridCol g c]
    lows = scanlS 0 unus
    higs = scanlS 0 (reverseL unus)
    rge cg = let k   = count0s cg - 1
                 lft = mn - sum cg
             in (lft - nth higs k, lft - nth lows k)
    mima (a,b) (c2,d) = (if a > c2 then a else c2, if b < d then b else d)
    fold1 f (x:xs) = go x xs where go a []     = a
                                   go a (y:ys) = go (f a y) ys
    fold1 _ []     = (0,0)
    (mi,ma) = fold1 mima (map rge cellGroups)

-- ([moves], len, r, c) of the emptiest cell
ffm :: [Int] -> [Int] -> ([Int],(Int,(Int,Int)))
ffm g unus =
  if null unus then ([],(0,(0,0)))
  else minBy openMap
  where
    openSquares = [(r,c) | r <- range 1 n, c <- range 1 n, at g r c == 0]
    openMap = [ (possibleMoves g unus r c,(r,c)) | (r,c) <- openSquares ]
    minBy (x:xs) = go x xs
      where go (bm,brc) [] = (bm,(length bm,brc))
            go (bm,brc) ((m,rc):ys) =
              if length m < length bm then go (m,rc) ys else go (bm,brc) ys
    minBy [] = ([],(0,(0,0)))

cmpG :: [Int] -> [Int] -> Int          -- lexicographic, upstream Ord tiebreak
cmpG []     []     = 0
cmpG (a:as) (b:bs) = if a < b then 0-1 else if a > b then 1 else cmpG as bs
cmpG _      _      = 0

-- queue entries: (priority, (grid, (unus, moveinfo)))
type Sq = (Int,([Int],([Int],([Int],(Int,(Int,Int))))))

insQ :: Sq -> [Sq] -> [Sq]
insQ e [] = [e]
insQ e@(p,(g,_)) l@(e2@(p2,(g2,_)):rest) =
  if p < p2 || (p == p2 && cmpG g g2 < 0) then e : l else e2 : insQ e rest

delL :: Int -> [Int] -> [Int]
delL _ [] = []
delL v (x:xs) = if x == v then xs else x : delL v xs

place :: [Int] -> [Int] -> Int -> Int -> Int -> Sq
place g unus r c k =
  let g2   = setAt g r c k
      uns  = delL k unus
      mv   = ffm g2 uns
      len  = fst (snd mv)
  in (length uns + len, (g2, (uns, mv)))

successors :: Sq -> [Sq]
successors (_,(g,(unus,(moves,(_,(r,c)))))) = map (place g unus r c) moves

bestFirst :: [Sq] -> [Int]
bestFirst [] = []
bestFirst (front@(p,(g,_)):queue) =
  if p == 0 then g
  else bestFirst (foldl (\q e -> insQ e q) queue (successors front))

bench :: Int
bench = foldl (\acc v -> v + acc*31) 0 (bestFirst [ini])
  where
    g0   = [0,0,0,0,0,0,0,0,0]
    u0   = range 1 9
    mv   = ffm g0 u0
    ini  = (2*n*n, (g0, (u0, mv)))

main :: Int
main = bench
