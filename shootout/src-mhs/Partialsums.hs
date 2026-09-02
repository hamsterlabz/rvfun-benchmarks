-- Partialsums.hs - shootout partialsums, fun-version companion (verbatim version:
-- src-verbatim/partialsums.hs, n = 500).  The same nine series in one
-- accumulator pass, FloatW throughout (Double -> Float rule); tt**(k-1)
-- becomes a RUNNING power threaded through the fold (p_k = p_{k-1}*tt,
-- p_1 = 1), the exact same values; sin/cos are the Taylor forms the
-- suite ports carry (Sphere.hs precedent; the machine FPU has no
-- transcendentals).  bench folds the nine sums scaled by 1e4.
module Partialsums where
import Prelude()
import NanoPrelude

n :: Int
n = 500

sinD_ :: FloatW -> FloatW
sinD_ x = go 0 x x
  where
    x2 = x *. x
    go k term acc =
      if k >= 12 then acc
      else let term2 = negateD term *. x2 /. fromIntD ((2*k+2) * (2*k+3))
           in go (k+1) term2 (acc +. term2)

cosD_ :: FloatW -> FloatW
cosD_ x = go 0 (fromIntD 1) (fromIntD 1)
  where
    x2 = x *. x
    go k term acc =
      if k >= 12 then acc
      else let term2 = negateD term *. x2 /. fromIntD ((2*k+1) * (2*k+2))
           in go (k+1) term2 (acc +. term2)

-- range-reduce into [-pi,pi) before the series (k runs to 500)
twoPi :: FloatW
twoPi = 6.2831853

reduce :: FloatW -> FloatW
reduce x = x -. fromIntD (truncateD (x /. twoPi)) *. twoPi

sinR, cosR :: FloatW -> FloatW
sinR = sinD_ . reduce
cosR = cosD_ . reduce

sigma :: Int -> FloatW -> FloatW -> (FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,FloatW))))))))
        -> (FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,(FloatW,FloatW))))))))
sigma i alt p (a1,(a2,(a3,(a4,(a5,(a6,(a7,(a8,a9))))))))
  | i > n     = (a1,(a2,(a3,(a4,(a5,(a6,(a7,(a8,a9))))))))
  | otherwise =
      let k  = fromIntD i
          k2 = k *. k
          k3 = k2 *. k
          sk = sinR k
          ck = cosR k
          tt = fromIntD 2 /. fromIntD 3
      in sigma (i+1) (negateD alt) (p *. tt)
           ( a1 +. p
           ,(a2 +. fromIntD 1 /. sqrtD k
           ,(a3 +. fromIntD 1 /. (k *. (k +. fromIntD 1))
           ,(a4 +. fromIntD 1 /. (k3 *. sk *. sk)
           ,(a5 +. fromIntD 1 /. (k3 *. ck *. ck)
           ,(a6 +. fromIntD 1 /. k
           ,(a7 +. fromIntD 1 /. k2
           ,(a8 +. alt /. k
           , a9 +. alt /. (fromIntD 2 *. k -. fromIntD 1)))))))))

hashF :: Int -> FloatW -> Int
hashF acc v = truncateD (v *. fromIntD 10000) + acc*31

bench :: Int
bench =
  let z = fromIntD 0
      (a1,(a2,(a3,(a4,(a5,(a6,(a7,(a8,a9)))))))) = sigma 1 (fromIntD 1) (fromIntD 1) (z,(z,(z,(z,(z,(z,(z,(z,z))))))))
  in foldl hashF 0 [a1,a2,a3,a4,a5,a6,a7,a8,a9]

main :: Int
main = bench
