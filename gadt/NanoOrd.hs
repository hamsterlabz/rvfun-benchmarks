-- NanoOrd.hs - the ordering class the nano dialect does not have.
--
-- NanoPrelude's (<)/(<=)/(>)/(>=) are monomorphic Int primitives, which is
-- deliberate: making them class methods costs a dictionary build plus a
-- selector reduction at every use, including at Int (NanoClasses' own note
-- measures +8.6% on Adjoxo).  A benchmark that orders something OTHER than
-- Int -- a Char-keyed trie, say -- still needs the class, and its upstream
-- body cannot be edited to say primCharLT.  Such a port imports this module
-- and hides the four Int operators from NanoClasses.
--
-- There is a matching ghc-shim/NanoOrd.hs that simply re-exports Prelude's
-- Ord, so one port source compiles on both versions.
module NanoOrd(Ord(..)) where
import Prelude()
import NanoPrelude hiding ((<), (<=), (>), (>=))
import Data.Char_Type (Char)   -- NanoPrelude exports Int, not Char
import Primitives (primIntLT, primIntLE, primIntGT, primIntGE,
                   primCharLT, primCharLE, primCharGT, primCharGE)

infix 4 <, <=, >, >=
class Ord a where
  (<)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>)  :: a -> a -> Bool
  (>=) :: a -> a -> Bool

instance Ord Int where
  (<)  = primIntLT
  (<=) = primIntLE
  (>)  = primIntGT
  (>=) = primIntGE

instance Ord Char where
  (<)  = primCharLT
  (<=) = primCharLE
  (>)  = primCharGT
  (>=) = primCharGE
