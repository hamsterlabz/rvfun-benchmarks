-- PORT (Double->Float rule, all versions binary32): Double -> Float,
-- nothing else changed from the upstream source.
{-# OPTIONS -O2 -fexcess-precision #-}
--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Translation of the Clean by Don Stewart
--

import System.Environment; import System.Exit
import Numeric
import Control.Monad -- PORT: GHC 9.6 compat

main = do n <- getArgs >>= readIO . head
          mapM_ draw $ sigma (n::Int) (1::Int) 1 (2/3) 0 0 0 0 0 0 0 0 0

draw (s,t) = putStrLn $ (showFFloat (Just 9) (s::Float) []) ++ "\t" ++ t

sigma !n !i !alt !tt !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8 !a9
    | i <= n    = sigma n (i+1) (-alt) tt
                     (a1 + tt ** (k-1))
                     (a2 + 1 / (sqrt k))
                     (a3 + 1 / (k * (k + 1)))
                     (a4 + 1 / (k3 * sk * sk))
                     (a5 + 1 / (k3 * ck * ck))
                     (a6 + 1 / k)
                     (a7 + 1 / k2)
                     (a8 + alt / k)
                     (a9 + alt / (2 * k - 1))

    | otherwise = [(a1, "(2/3)^k"),     (a2, "k^-0.5"),        (a3, "1/k(k+1)")
                  ,(a4, "Flint Hills"), (a5, "Cookson Hills"), (a6, "Harmonic")
                  ,(a7, "Riemann Zeta"),(a8, "Alternating Harmonic"), (a9, "Gregory")]

    where k  = fromIntegral i
          k3 = k2*k; k2 = k*k; sk = sin k; ck = cos k
