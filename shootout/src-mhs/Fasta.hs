-- Fasta.hs - shootout fasta, fun-version companion of src-verbatim/fasta.hs
-- at the same N = 250.  No IO on the fun backend, so the SAME three
-- sequences are GENERATED (ALU repeated for 2N; IUB and homosapiens
-- LCG-driven for 3N and 5N) and hashed instead of printed; the LCG is
-- upstream's (seed' = (seed*3877+29573) mod 139968), probabilities ride
-- FloatW (Double -> Float, the porting rule).
module Fasta where
import Prelude()
import NanoPrelude
import Primitives (Char, primOrd)

alu :: [Char]
alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iubC :: [Char]
iubC = "acgtBDHKMNRSVWY"
iubP :: [FloatW]
iubP = [0.27, 0.12, 0.12, 0.27, 0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02]

homC :: [Char]
homC = "acgt"
homP :: [FloatW]
homP = [0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008]

cumul :: [FloatW] -> [FloatW]
cumul ps = go (fromIntD 0) ps
  where go _ []     = []
        go a (p:qs) = let s = a +. p in s : go s qs

im, ia, ic :: Int
im = 139968
ia = 3877
ic = 29573

nextSeed :: Int -> Int
nextSeed s = mod (s * ia + ic) im

pick :: [Char] -> [FloatW] -> FloatW -> Char
pick (c:_)  []     _ = c
pick (c:cs) (p:ps) r = if r <. p then c else pick cs ps r
pick []     _      _ = 'a'

hashC :: Int -> Char -> Int
hashC acc c = primOrd c + acc*31

-- ONE: alu repeated to 2N chars
repHash :: Int -> Int
repHash n = go n alu 0
  where go 0 _      acc = acc
        go k []     acc = go k alu acc
        go k (c:cs) acc = go (k-1) cs (hashC acc c)

-- random sections: fold n draws, returning (hash, final seed)
rndHash :: [Char] -> [FloatW] -> Int -> Int -> Int -> (Int, Int)
rndHash cs cps n seed acc = go n seed acc
  where go 0 s a = (a, s)
        go k s a = let s' = nextSeed s
                       r  = fromIntD s' /. fromIntD im
                   in go (k-1) s' (hashC a (pick cs cps r))

n :: Int
n = 250

bench :: Int
bench = h3
  where
    h1        = repHash (2*n)
    (h2, s2)  = rndHash iubC (cumul iubP) (3*n) 42 h1
    (h3, _)   = rndHash homC (cumul homP) (5*n) s2 h2

main :: Int
main = bench
