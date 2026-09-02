-- Methcall.hs - shootout methcall, fun-version companion (verbatim version:
-- src-verbatim/methcall.hs, n = 20000).  n activate-then-value calls on a
-- Toggle, then on an NthToggle; dynamic dispatch rides the shared sum
-- type (the nano dialect has no typeclass IO objects); every value call
-- is consumed by the fold so none can be elided.
module Methcall where
import Prelude()
import NanoPrelude

data Toggle = T Bool | NT Bool Int Int

activate :: Toggle -> Toggle
activate (T s)      = T (not s)
activate (NT s v m) = if v+1 == m then NT (not s) 0 m else NT s (v+1) m

value :: Toggle -> Bool
value (T s)      = s
value (NT s _ _) = s

callN :: Int -> Toggle -> Int -> Int
callN 0 t acc = acc*2 + (if value t then 1 else 0)
callN k t acc = let t' = activate t
                in callN (k-1) t' (acc + (if value t' then 1 else 0))

n :: Int
n = 20000

bench :: Int
bench = callN n (T True) 0 * 31 + callN n (NT True 0 3) 0

main :: Int
main = bench
