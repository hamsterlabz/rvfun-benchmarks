-- Ary.hs - shootout ary, fun-version companion (verbatim version:
-- src-verbatim/ary.hs, n = 100).  The same computation on the dialect's
-- IOArray (TArr precedent): a[i] = i+1, 1000 passes of b[i] += a[i],
-- result y[0] and y[n-1] folded with the suite hash.
module Ary(main) where
import Prelude
import Data.IOArray

n :: Int
n = 100

main :: IO ()
main = do
  a <- newIOArray n 0
  b <- newIOArray n 0
  let fill i = if i >= n then return () else writeIOArray a i (i+1) >> fill (i+1)
      addA i = if i < 0 then return ()
               else do ai <- readIOArray a i
                       bi <- readIOArray b i
                       writeIOArray b i (ai+bi)
                       addA (i-1)
      passes k = if k == 0 then return () else addA (n-1) >> passes (k-1)
  fill 0
  passes 1000
  y1 <- readIOArray b 0
  yn <- readIOArray b (n-1)
  putStrLn (show (y1*31 + yn))
