-- Data/Integer_Type.hs - nano SHADOW of the mhs lib module (the -i search
-- order puts the suite first).  mhs desugars an overloaded numeric literal
-- through Data.Integer_Type._intToInteger whenever a fromInteger is in
-- scope; the lib module is real bignums and drags Control.Error and the
-- full Eq/Int hierarchy into the image, which the fun --bare build cannot
-- carry.  Every literal in the ported benchmarks fits Int, so Integer here
-- is an Int wrapper and nothing more.
module Data.Integer_Type(module Data.Integer_Type) where
import Prelude()
import NanoPrelude (Int)
import Data.Bool_Type
import Data.Eq

data Integer = MkInteger Int

_intToInteger :: Int -> Integer
_intToInteger i = MkInteger i

_integerToInt :: Integer -> Int
_integerToInt (MkInteger i) = i

-- literal patterns at Integer desugar to (==)
instance Eq Integer where
  MkInteger a == MkInteger b = eqI a b
    where eqI :: Int -> Int -> Bool
          eqI = primitive "=="
  a /= b = case a == b of { True -> False; False -> True }
