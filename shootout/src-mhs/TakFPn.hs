-- TakFPn.hs - the Takeuchi function over floats, mirroring benches/takfp.ml of
-- min-caml-hs line for line: same recursion, same 18 12 6, value 7.
-- NanoPrelude dialect: the fun backend on this target is FullPrelude-free and
-- main is a pure Int that the report epilogue prints.
module TakFPn where
import Prelude()
import NanoPrelude

tak :: Float -> Float -> Float -> Float
tak x y z = if y >=. x then z
            else tak (tak (x -. 1.0) y z) (tak (y -. 1.0) z x) (tak (z -. 1.0) x y)

main :: Int
main = truncateD (tak 18.0 12.0 6.0)
