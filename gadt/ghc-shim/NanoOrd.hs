{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim for NanoOrd: under GHC the real Prelude already has Ord, so this
-- just re-exports it.  See gadt/NanoOrd.hs for why the mhs version needs its own.
module NanoOrd(Ord(..)) where
import Prelude(Ord(..))
