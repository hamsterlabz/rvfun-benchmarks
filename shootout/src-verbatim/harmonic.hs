{-# OPTIONS -fglasgow-exts #-}
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Contributed by Greg Buchholz
-- Enhanced by Einar Karttunen, Mirko Rahn and Don Stewart

import System.Environment; import Numeric; import GHC.Base -- PORT: isTrue# comes from here in 9.6; import GHC.Float -- PORT: Double->Float rule (all versions binary32): D#->F#, ##-ops -> Float# primops

main = putStrLn . (\(I# n) -> showFFloat (Just 9) (F# (loop n 0.0#)) []) . read . head =<< getArgs

-- PORT: GHC 7.8 changed (==#) from Bool to Int#; isTrue# is the mechanical
-- modern spelling.  Arithmetic untouched.
loop d s = if isTrue# (d ==# 0#) then s else loop (d-#1#) (s `plusFloat#` (1.0# `divideFloat#` int2Float# d))

