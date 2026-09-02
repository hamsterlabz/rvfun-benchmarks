-- NanoRead.hs - the Read class the nano dialect does not have.
--
-- Same reasoning as NanoOrd: NanoClasses carries Eq and Show because
-- `deriving` keys on them, but nothing in the nano world reads.  A port whose
-- upstream body says `reads` -- a parser-driven benchmark such as nofib
-- real/infer, which reads Terms, MonoTypes, PolyTypes and an Env -- needs the
-- class, and that body cannot be edited to name a monomorphic parser instead.
--
-- `read` is defined HERE, not taken from Prelude on the GHC side, and the
-- ghc-shim twin repeats this definition verbatim.  Prelude's read insists on a
-- unique complete parse and calls it an error otherwise; this one takes the
-- first parse whose remainder is blank.  Either rule is defensible, but the
-- two versions have to apply the SAME one or a lockstep comparison is meaningless.
module NanoRead(Read(..), ReadS, reads, read) where
import Prelude()
import NanoPrelude
import Data.Char_Type (Char, String)   -- NanoPrelude exports Int, not Char
import Primitives (primCharEQ, primChr)

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec :: Int -> ReadS a

reads :: Read a => ReadS a
reads = readsPrec 0

read :: Read a => String -> a
read s = go (reads s)
  where
    go ((x,t) : rest) = if all blank t then x else go rest
    go []             = readFailed
    blank c = primCharEQ c (primChr 32) || primCharEQ c (primChr 9)
           || primCharEQ c (primChr 10) || primCharEQ c (primChr 13)

readFailed :: a
readFailed = readFailed
