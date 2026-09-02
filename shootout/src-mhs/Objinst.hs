-- Objinst.hs - shootout objinst, fun-version companion (verbatim version:
-- src-verbatim/objinst.hs, n = 20000).  The upstream work: 5 activations
-- of a Toggle + n Toggle constructions, 8 activations of an NthToggle +
-- n NthToggle constructions; IORef state becomes pure state threading,
-- construction forced by summing each object's value.
module Objinst where
import Prelude()
import NanoPrelude

data Toggle = T Bool | NT Bool Int Int

activate :: Toggle -> Toggle
activate (T s)      = T (not s)
activate (NT s v m) = if v+1 == m then NT (not s) 0 m else NT s (v+1) m

value :: Toggle -> Bool
value (T s)      = s
value (NT s _ _) = s

actN :: Int -> Toggle -> (Int, Toggle)
actN 0 t = (0, t)
actN k t = let t' = activate t
               (h, tf) = actN (k-1) t'
           in ((if value t' then 1 else 0) + h*2, tf)

allocN :: Int -> Toggle -> Int
allocN 0 _ = 0
allocN k proto = (if value (mk proto) then 1 else 0) + allocN (k-1) proto
  where mk (T s)      = T s
        mk (NT s v m) = NT s v m

n :: Int
n = 20000

bench :: Int
bench = h1*31 + a1*31 + h2*31 + a2
  where (h1, _) = actN 5 (T True)
        a1      = allocN n (T True)
        (h2, _) = actN 8 (NT True 0 3)
        a2      = allocN n (NT True 0 3)

main :: Int
main = bench
