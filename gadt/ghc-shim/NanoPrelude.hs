{-# LANGUAGE NoImplicitPrelude #-}
-- GHC shim: the gadt suite's NanoPrelude is just monomorphic-Int Prelude; under
-- GHC we re-export the real (polymorphic) Prelude. W=Word32 (Common) then gives
-- the 32-bit-unsigned semantics for free; Int counters stay signed.
module NanoPrelude (module Prelude, module NanoPrelude) where
import Prelude

-- FloatW version of the shim: the fun backend's FloatW is the machine float
-- (RV32F, IEEE binary32), so under GHC it is Float, NOT Double. Same IEEE
-- operations in the same order on both backends, so results stay
-- bit-comparable. The dot-suffixed names mirror the mhs NanoPrelude atoms.
type FloatW = Float

infixl 6 +., -.
infixl 7 *., /.
(+.), (-.), (*.), (/.) :: Float -> Float -> Float
(+.) = (+); (-.) = (-); (*.) = (*); (/.) = (/)
infix 4 ==., /=., <., <=., >., >=.
(==.), (/=.), (<.), (<=.), (>.), (>=.) :: Float -> Float -> Bool
(==.) = (==); (/=.) = (/=); (<.) = (<); (<=.) = (<=); (>.) = (>); (>=.) = (>=)
negateD, absD, sqrtD, sinD, cosD :: Float -> Float
negateD = negate; absD = abs; sqrtD = sqrt
-- sin/cos are real blobs on the fun target, so the shim needs them too.
sinD = sin; cosD = cos
fromIntD :: Int -> Float
fromIntD = fromIntegral
truncateD :: Float -> Int
truncateD = truncate
maxD, minD :: Float -> Float -> Float
maxD = max; minD = min
