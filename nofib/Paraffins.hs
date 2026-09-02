-- Paraffins.hs - nofib imaginary/paraffins, NanoPrelude dialect.
-- Verbatim algorithm (Steve Heller's Id example via MIT/RPaul/SLPJ).
-- Two pieces of glue, no restructuring:
--   * the self-referential `Array Int [Radical]` becomes a self-referential
--     LIST indexed by idx; laziness memoises it exactly as the array did,
--     which is the whole point of the benchmark.
--   * four_partitions' `ceiling (m/2)` is written `div (m+1) 2` -- the same
--     integer, without dragging in the float path.
-- The nofib main prints four lists; bench sums them, the usual hash here.
module Paraffins where
import Prelude()
import NanoPrelude

data Radical = H | C Radical Radical Radical

data Paraffin = BCP Radical Radical | CCP Radical Radical Radical Radical

idx :: [a] -> Int -> a
idx (x:_)  0 = x
idx (_:xs) n = idx xs (n-1)

enumTo :: Int -> Int -> [Int]
enumTo lo hi = if lo > hi then [] else lo : enumTo (lo+1) hi

three_partitions :: Int -> [(Int,Int,Int)]
three_partitions m =
  concat (map (\i ->
    concat (map (\j -> [(i, j, m - (i+j))])
                (enumTo i (div (m-i) 2))))
    (enumTo 0 (div m 3)))

four_partitions :: Int -> [(Int,Int,Int,Int)]
four_partitions m =
  concat (map (\i ->
    concat (map (\j ->
      concat (map (\k -> [(i, j, k, m - (i+j+k))])
                  (enumTo (maxI j (div (m+1) 2 - i - j)) (div (m-i-j) 2))))
      (enumTo i (div (m-i) 3))))
    (enumTo 0 (div m 4)))

maxI :: Int -> Int -> Int
maxI a b = if a > b then a else b

remainders :: [a] -> [[a]]
remainders []     = []
remainders (r:rs) = (r:rs) : remainders rs

-- the array of radicals by size, as a lazy self-referential list
radical_generator :: Int -> [[Radical]]
radical_generator n = radicals
  where radicals = [H] : map (rads_of_size_n radicals) (enumTo 1 n)

rads_of_size_n :: [[Radical]] -> Int -> [Radical]
rads_of_size_n radicals n =
  concat (map (\ijk -> case ijk of
    (i,j,k) ->
      concat (map (\ris -> case ris of
        (ri:_) ->
          concat (map (\rjs -> case rjs of
            (rj:_) ->
              map (\rk -> C ri rj rk)
                  (if j == k then rjs else idx radicals k)
            [] -> [])
            (remainders (if i == j then ris else idx radicals j)))
        [] -> [])
        (remainders (idx radicals i))))
    (three_partitions (n-1)))

bcp_generator :: [[Radical]] -> Int -> [Paraffin]
bcp_generator radicals n =
  if mod n 2 /= 0 then []
  else concat (map (\r1s -> case r1s of
                      (r1:_) -> map (\r2 -> BCP r1 r2) r1s
                      []     -> [])
                   (remainders (idx radicals (div n 2))))

ccp_generator :: [[Radical]] -> Int -> [Paraffin]
ccp_generator radicals n =
  concat (map (\ijkl -> case ijkl of
    (i,j,k,l) ->
      concat (map (\ris -> case ris of
        (ri:_) ->
          concat (map (\rjs -> case rjs of
            (rj:_) ->
              concat (map (\rks -> case rks of
                (rk:_) ->
                  map (\rl -> CCP ri rj rk rl)
                      (if k == l then rks else idx radicals l)
                [] -> [])
                (remainders (if j == k then rjs else idx radicals k)))
            [] -> [])
            (remainders (if i == j then ris else idx radicals j)))
        [] -> [])
        (remainders (idx radicals i))))
    (four_partitions (n-1)))

bcp_until :: Int -> [Int]
bcp_until n = map (\j -> length (bcp_generator radicals j)) (enumTo 1 n)
  where radicals = radical_generator (div n 2)

ccp_until :: Int -> [Int]
ccp_until n = map (\j -> length (ccp_generator radicals j)) (enumTo 1 n)
  where radicals = radical_generator (div n 2)

paraffins_until :: Int -> [Int]
paraffins_until n =
  map (\j -> length (bcp_generator radicals j) + length (ccp_generator radicals j))
      (enumTo 1 n)
  where radicals = radical_generator (div n 2)

-- SIM SCALE (user ruling 2026-07-30): fast* is the upstream FAST value,
-- sim* is what is actually run; both backends compile the same one.
fastN, simN :: Int
fastN = 11
simN = 8

bench :: Int
bench = sum (map (\i -> length (idx rads i)) (enumTo 0 simN))
      + sum (bcp_until simN)
      + sum (ccp_until simN)
      + sum (paraffins_until simN)
  where rads = radical_generator simN

main :: Int
main = bench
