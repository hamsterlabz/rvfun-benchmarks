{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim for NanoClasses. Under mhs, NanoClasses is NanoPrelude plus the
-- Eq/Show classes; under GHC the real Prelude already has them, so this just
-- re-exports it -- MINUS the names a class-needing port defines for itself
-- (the fun version has no take/head/error/..., so the port carries them, and the
-- same single source file must compile on both versions).
module NanoClasses(module P, module NP, primOrd, primChr) where
import Prelude as P hiding
  ( (++), head, tail, take, drop, dropWhile, reverse, repeat
  , elem, notElem, and, or, max, words, error, fromEnum, toEnum, (^)
  , zipWith, transpose, until, splitAt, quotRem )   -- circsim glue defines these
-- the float atoms live in NanoPrelude; a NanoClasses-importing port (the
-- hartel ones) still names them, so re-export them explicitly.  An explicit
-- list, not a wildcard, or Prelude would come through twice.
import NanoPrelude as NP
  ( FloatW, (+.), (-.), (*.), (/.), (==.), (/=.), (<.), (<=.), (>.), (>=.)
  , negateD, absD, sqrtD, sinD, cosD, fromIntD, truncateD, maxD, minD )
import qualified Data.Char as DC


primOrd :: Char -> Int
primOrd = DC.ord

primChr :: Int -> Char
primChr = DC.chr
