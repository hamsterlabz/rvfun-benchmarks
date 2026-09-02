{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim for NanoBits: the mhs original is the nano-visible door to the
-- backend's bitwise blobs; under GHC the same operations come from Data.Bits.
-- Shifts are the LOGICAL ones the fun blobs implement (_prim_sll/_prim_srl),
-- so shrI goes through Word to avoid Int's arithmetic right shift.
module NanoBits(andI, orI, xorI, shlI, shrI) where
import Prelude
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Word (Word32)

andI :: Int -> Int -> Int
andI a b = a .&. b
orI :: Int -> Int -> Int
orI a b = a .|. b
xorI :: Int -> Int -> Int
xorI a b = a `xor` b
shlI :: Int -> Int -> Int
shlI a n = a `shiftL` n
shrI :: Int -> Int -> Int
shrI a n = fromIntegral ((fromIntegral a :: Word32) `shiftR` n)
