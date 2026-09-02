{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim for NanoArray.  The mhs original wraps pure CPS array primitives
-- that the fun backend implements as blobs; under GHC the same shapes go to a
-- real mutable IOArray through unsafePerformIO.  A list-backed stand-in would
-- compile but would make the GHC version artificially slow, and this version is a
-- measured competitor, not just an oracle.
--
-- Every operation is NOINLINE and every read is forced AT THE CALL.  Without
-- both, -O2 treats `unsafePerformIO (readArray a i)` as the pure value it
-- claims to be: it floats reads out of loops, CSEs repeated ones, and lets a
-- read's thunk drift past later writes.  The measured effect was not subtle --
-- the GHC version returned 0 for heapsort/floydwarshall/matrixm/alloc/fannkuch,
-- 4 for queens, 4095 for sieve, -1 for crc32, and ran fannkuch in 33.5k cycles
-- against the 7.0M the program actually needs.  NOINLINE makes the call opaque
-- so nothing is hoisted or shared, and forcing the read before the
-- continuation pins it in the same order the CPS chain has on the reducer.
module NanoArray(IOArray, newArr, readArr, writeArr, sizeArr, copyArr) where
import Prelude
import qualified Data.Array.IO as A
import Data.Array.MArray (newArray, readArray, writeArray, getBounds, getElems)
import System.IO.Unsafe (unsafePerformIO)

type IOArray a = A.IOArray Int a

-- Each operation returns its continuation FROM INSIDE the IO action, so the
-- array access has to run before the result exists.  The obvious spelling,
--     writeArr a i x k = unsafePerformIO (writeArray a i x) `seq` k
-- does not survive -O1: the strictness analyser sees a pure `()` scrutinee and
-- drops the seq, the write with it.  (Measured: -fno-strictness gave the right
-- answer, -fno-cse -fno-full-laziness -fno-state-hack did not.)  NOINLINE keeps
-- the call opaque so nothing is hoisted or shared across iterations.

{-# NOINLINE newArr #-}
newArr :: Int -> a -> (IOArray a -> b) -> b
newArr n x k = unsafePerformIO (do { a <- newArray (0, n - 1) x; return (k a) })

{-# NOINLINE readArr #-}
readArr :: IOArray a -> Int -> (a -> b) -> b
readArr a i k = unsafePerformIO (do { v <- readArray a i; return (k v) })

{-# NOINLINE writeArr #-}
writeArr :: IOArray a -> Int -> a -> b -> b
writeArr a i x k = unsafePerformIO (do { writeArray a i x; return k })

{-# NOINLINE sizeArr #-}
sizeArr :: IOArray a -> (Int -> b) -> b
sizeArr a k = unsafePerformIO (do { (lo, hi) <- getBounds a; return (k (hi - lo + 1)) })

{-# NOINLINE copyArr #-}
copyArr :: IOArray a -> (IOArray a -> b) -> b
copyArr a k = unsafePerformIO (do
  { es <- getElems a
  ; (lo, hi) <- getBounds a
  ; c <- case es of
           []      -> newArray (lo, hi) (error "copyArr: empty array")
           (e : _) -> newArray (lo, hi) e
  ; mapM_ (\(i, e) -> writeArray c i e) (zip [lo .. hi] es)
  ; return (k c) })
