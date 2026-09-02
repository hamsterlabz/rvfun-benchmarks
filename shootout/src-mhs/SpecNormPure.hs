-- SpecNormPure - spectralnorm with NO arrays and NO mutable state: a vector is
-- a list of Float and every step rebuilds one.  Same problem and same answer as
-- the array version (n = 8, ten power iterations, 1270046208): the shootout
-- entry keeps three mutable buffers and writes through them, which is the shape
-- a graph reducer has to fight; this one is ordinary cons cells and folds.
module SpecNormPure where
import Prelude()
import NanoPrelude

n :: Int
n = 8

upto :: Int -> Int -> [Int]
upto i m = if i > m then [] else i : upto (i + 1) m

idx :: [Int]
idx = upto 0 (n - 1)

-- A(i,j) = 1 / ((i+j)(i+j+1)/2 + i + 1)
aij :: Int -> Int -> Float
aij i j = let s = i + j
          in fromIntD 1 /. fromIntD (div (s * (s + 1)) 2 + i + 1)

-- one row of A  . x   (row index fixed, walk the vector)
rowDot :: Int -> [Float] -> Int -> Float -> Float
rowDot i xs j acc = case xs of
  []       -> acc
  (y : ys) -> rowDot i ys (j + 1) (acc +. aij i j *. y)

-- one row of A^T . x
colDot :: Int -> [Float] -> Int -> Float -> Float
colDot i xs j acc = case xs of
  []       -> acc
  (y : ys) -> colDot i ys (j + 1) (acc +. aij j i *. y)

av :: [Float] -> [Float]
av x = map (\i -> rowDot i x 0 (fromIntD 0)) idx

atv :: [Float] -> [Float]
atv x = map (\i -> colDot i x 0 (fromIntD 0)) idx

atav :: [Float] -> [Float]
atav x = atv (av x)

-- the C loop runs ten times: v = AtAv u ; u = AtAv v
step :: [Float] -> [Float]
step u = atav (atav u)

applyN :: Int -> [Float] -> [Float]
applyN k u = if k <= 0 then u else applyN (k - 1) (step u)

zipDot :: [Float] -> [Float] -> Float -> Float
zipDot xs ys acc = case xs of
  []       -> acc
  (a : as) -> case ys of
                []       -> acc
                (b : bs) -> zipDot as bs (acc +. a *. b)

u0 :: [Float]
u0 = map (\_ -> fromIntD 1) idx

bench :: Int
bench =
  let u9  = applyN 9 u0
      vv' = atav u9          -- the tenth iteration's v
      uu' = atav vv'         -- and its u
      vbv = zipDot uu' vv' (fromIntD 0)
      vsq = zipDot vv' vv' (fromIntD 0)
  in truncateD (sqrtD (vbv /. vsq) *. fromIntD 1000000000)

main :: Int
main = bench
