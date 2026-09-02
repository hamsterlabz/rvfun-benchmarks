-- GHC shim for the suite-local nano Data.Integer_Type shadow: on the GHC
-- version Integer is the real thing and the two conversions are the obvious ones.
module Data.Integer_Type (Integer, _integerToInt, _intToInteger) where

_integerToInt :: Integer -> Int
_integerToInt = fromInteger

_intToInteger :: Int -> Integer
_intToInteger = toInteger
