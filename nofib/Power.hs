-- Power.hs - nofib gc/power (McIlroy, "Power series, power serious",
-- JFP 9(3) 1999), NanoClasses dialect.  THE BENCHMARK BODY IS UPSTREAM'S,
-- UNTOUCHED: every declaration below the glue block is Main.hs verbatim,
-- minus its module head, imports, `default` declaration, IO main and the
-- Show (Ps a) instance (print machinery of the dropped wrapper; its
-- showsPrec p [0] literal is only typeable through the dropped `default`
-- declaration), with
-- its tabs expanded to the standard 8-column stops (token-identical for
-- GHC; mhs layout reads a tab as one column).  The
-- glue is this header -- the three numeric classes the body's instances
-- populate (mhs NanoClasses carries only Eq and Show), their FloatW
-- instances over the NanoPrelude float atoms, (^) and error -- and the
-- entry point at the bottom.  Overloaded literals ride the suite-local
-- Data/Integer_Type.hs shadow (Integer as an Int wrapper).
-- The GHC version compiles the same body against GHC's own Num/Fractional/
-- Floating: the glue class block is stripped from the shadow copy
-- (GHC-ARM-STRIP markers, build-nofib.sh).
--
-- Upstream prints extract n of four series (n = FAST_OPTS = 50), with
-- coefficients defaulted to Rational.  The porting doc lists power out of
-- dialect over exactly that Rational; ported on user order (2026-09-01)
-- with the coefficient type pinned to FloatW instead -- binary32 is what
-- the machine has, Rational is Integer territory.
-- SIM SCALE (user ruling 2026-07-30 convention): fastN = 50 terms,
-- simN = 12 (the series operators are O(n^2)+ and the ts/tree Catalan
-- coefficients leave Int range past ~15 terms).  Consumption: the two
-- fractional series fold truncateD (c *. 1e6), the two integer-valued
-- series fold truncateD c, all through the nofib hash.
module Power where
import Prelude()
import NanoClasses
import Data.Integer_Type (Integer, _integerToInt, _intToInteger)

-- GHC-ARM-STRIP-BEGIN
infixl 6 +, -
infixl 7 *, /

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Integer -> a
  a - b = a + negate b

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  recip a = fromInteger 1 / a

class Fractional a => Floating a where
  sqrt :: a -> a

-- a literal PATTERN at Integer desugars through fromInteger at Integer
instance Num Integer where
  a + b = _intToInteger (_integerToInt a + _integerToInt b)
  a - b = _intToInteger (_integerToInt a - _integerToInt b)
  a * b = _intToInteger (_integerToInt a * _integerToInt b)
  negate a = _intToInteger (negate (_integerToInt a))
  fromInteger n = n

-- the glue class captures every unqualified (+)/(-)/(*) in the module,
-- the Int counters of the body and the entry glue included
instance Num Int where
  (+) = primitive "+"
  (-) = primitive "-"
  (*) = primitive "*"
  negate n = 0 - n
  fromInteger = _integerToInt

instance Num FloatW where
  (+) = (+.)
  (-) = (-.)
  (*) = (*.)
  negate = negateD
  fromInteger n = fromIntD (_integerToInt n)

instance Fractional FloatW where
  (/) = (/.)

instance Floating FloatW where
  sqrt = sqrtD

instance Eq FloatW where
  (==) = (==.)
  (/=) = (/=.)

instance Show FloatW where
  showsPrec _ f s = showsPrec 0 (truncateD (f *. 1000000.0)) s
-- GHC-ARM-STRIP-END

infixr 8 ^
(^) :: Num a => a -> Int -> a
b ^ e = if e <= 0 then fromInteger 1 else b * (b ^ (e-1))

take :: Int -> [a] -> [a]
take k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : take (k-1) ys }

bottom :: a
bottom = bottom

error :: String -> a
error _ = bottom

-- upstream Main.hs, verbatim -------------------------------------------
infixl 7 .*
infixr 5 :+:
-- From Section 6
tree   = 0 :+: forest
forest = compose list tree
list   = 1 :+: list

ts = 1 :+: ts^2
        

-- The main implementation follows
data Ps a = Pz | a :+: Ps a

extract :: Int -> Ps a -> [a]
extract 0 ps         = []
extract n Pz         = []
extract n (x :+: ps) = x : extract (n-1) ps

deriv:: Num a => Ps a -> Ps a
integral:: Fractional a => Ps a -> Ps a
compose:: (Eq a, Num a) => Ps a -> Ps a -> Ps a
revert:: (Eq a, Fractional a) => Ps a -> Ps a
toList:: Num a => Ps a -> [a]
takePs:: Num a => Int -> Ps a -> [a]
(.*):: Num a => a -> Ps a -> Ps a
x:: Num a => Ps a
expx, sinx, cosx:: Fractional a => Ps a

c .* Pz = Pz
c .* (f :+: fs) = c*f :+: c.*fs

x = 0 :+: 1 :+: Pz

toList Pz = []                                          --(0)
toList (f :+: fs) = f : (toList fs)

takePs n fs = take n (toList fs)

instance (Eq a, Num a) => Eq (Ps a) where                       --(1)
        Pz == Pz = True
        Pz == (f :+: fs) = f==0 && Pz==fs
        fs == Pz = Pz==fs
        (f :+: fs) == (g :+: gs) = f==g && fs==gs

instance Num a => Num (Ps a) where
        negate Pz = Pz
        negate (f :+: fs) = -f :+: -fs

        Pz + gs = gs
        fs + Pz = fs
        (f :+: fs) + (g :+: gs) = f+g :+: fs+gs

        Pz * _ = Pz
        _ * Pz = Pz
        (f :+: fs) * (g :+: gs) =
                f*g :+: f.*gs + g.*fs + x*fs*gs         --(3)

        fromInteger 0 = Pz
        fromInteger c = fromInteger c :+: Pz

instance (Eq a, Fractional a) => Fractional (Ps a) where
        recip fs = 1/fs

        Pz/Pz = error "power series 0/0"
        Pz / (0 :+: gs) = Pz / gs
        Pz / _ = Pz
        (0 :+: fs) / (0 :+: gs) = fs / gs
        (f :+: fs) / (g :+: gs) = let q = f/g in
                q :+: (fs - q.*gs)/(g :+: gs)

compose Pz _ = Pz
compose (f :+: _) Pz = f :+: Pz
compose (f :+: fs) (0 :+: gs) = f :+: gs*(compose fs (0 :+: gs))
compose (f :+: fs) gs = (f :+: Pz) + gs*(compose fs gs) --(4)

revert (0 :+: fs) = rs where
        rs = 0 :+: 1/(compose fs rs)
revert (f0 :+: f1 :+: Pz) = -1/f1 :+: 1/f1 :+: Pz       --(5)

deriv Pz = Pz
deriv (_ :+: fs) = deriv1 fs 1 where
        deriv1 Pz _ = Pz
        deriv1 (f :+: fs) n = n*f :+: (deriv1 fs (n+1))

integral fs = 0 :+: (int1 fs 1) where                   --(6)
        int1 Pz _ = Pz
        int1 (f :+: fs) n = f/n :+: (int1 fs (n+1))

instance (Eq a, Fractional a) => Floating (Ps a) where
        sqrt Pz = Pz
        sqrt (0 :+: 0 :+: fs) = 0 :+: (sqrt fs)
        sqrt (1 :+: fs) = qs where
                qs = 1 + integral((deriv (1:+:fs))/(2.*qs))

expx = 1 + (integral expx)
sinx = integral cosx
cosx  = 1 - (integral sinx)

--(0) Convert power series to a list; used in printing.
--(1) Equality works on polynomials; diverges otherwise.
--(2) Specifies how to print the new data type.
--(3) x*fs*gs replaces 0:fs*gs to avoid extra zero
--    at end of product of polynomials; it works
--    because x is a finite series.
--(4) This extra production works for the composition
--    of polynomials with non-zero constant term,
--    but not for infinite series.
--(5) Special case for reverting a linear function.
--(6) There is no special case for (integral Pz)
--    because this would defeat the property that
--    (integral) emits one term before evaluating
--    its operand--a property used in solving
--    differential equations.


-- entry point (upstream: four putStrLn (show (extract p ...)), p = 50)
fastN, simN :: Int
fastN = 50
simN = 12

hashI :: [Int] -> Int -> Int
hashI vs acc0 = foldl (\acc v -> v + acc*31) acc0 vs

bench :: Int
bench = h4
  where
    s1, s2, s3, s4 :: Ps FloatW
    s1 = sinx - sqrt (1-cosx^2)
    s2 = sinx/cosx - revert (integral (1/(1+x^2)))
    s3 = ts
    s4 = tree
    h1 = hashI (map (\c -> truncateD (c *. 1000000.0)) (extract simN s1)) 0
    h2 = hashI (map (\c -> truncateD (c *. 1000000.0)) (extract simN s2)) h1
    h3 = hashI (map truncateD (extract simN s3)) h2
    h4 = hashI (map truncateD (extract simN s4)) h3

main :: Int
main = bench
