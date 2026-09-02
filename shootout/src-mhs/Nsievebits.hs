-- Nsievebits.hs - shootout nsievebits, fun-version companion (verbatim version:
-- src-verbatim/nsievebits.hs at argv 2: sieves 40000, 20000, 10000 -- the
-- smallest argument the Haskell version accepts, 2^(n-2) reaches a negative
-- exponent below it).  Bool IOArray sieve, counts folded with the suite
-- hash.
module Nsievebits(main) where
import Prelude
import Data.IOArray

sieve :: Int -> IO Int
sieve m = do
  a <- newIOArray (m+1) True
  let go i c =
        if i > m then return c
        else do e <- readIOArray a i
                if e then clear (i+i) >> go (i+1) (c+1) else go (i+1) c
        where clear j = if j > m then return ()
                        else writeIOArray a j False >> clear (j+i)
  go 2 0
main :: IO ()
main = do
  c1 <- sieve 40000
  c2 <- sieve 20000
  c3 <- sieve 10000
  putStrLn (show ((c1*31 + c2)*31 + c3))
