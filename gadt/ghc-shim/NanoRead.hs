{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim for NanoRead: the class is Prelude's, but `read` is NOT -- it is
-- the same definition as gadt/NanoRead.hs, so both versions agree on what counts
-- as a complete parse.  See there for why.
module NanoRead(Read(..), ReadS, reads, read) where
import Prelude(Read(..), ReadS, reads, String, Char, Bool, all, (||), (==))

read :: Read a => String -> a
read s = go (reads s)
  where
    go ((x,t) : rest) = if all blank t then x else go rest
    go []             = readFailed
    blank :: Char -> Bool
    blank c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

readFailed :: a
readFailed = readFailed
