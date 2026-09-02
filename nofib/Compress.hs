-- Compress.hs - nofib real/compress (Welch's LZW, Paul Sanders, BT Labs
-- 1992), NanoClasses dialect.  THE BENCHMARK BODY IS UPSTREAM'S, UNTOUCHED:
-- the declarations below the glue block are Defaults.hs, PTTrees.hs,
-- BinConv.hs and Encode.hs verbatim, minus their module headers.  The port is
-- the glue header, the FAST stdin as a string constant, and the entry point.
--
-- Upstream main: forM_ [1..200] $ \n -> print (hash (compress (take (len -
-- (n `mod` 31)) input))) over compress.faststdin (1944 bytes).
-- SIM SCALE (same ruling as Grep): fastN = 200 repetitions; simN = 1.
--
-- One thing the dialect has to supply: upstream compares Chars with (<)/(>)
-- in the trie (Encode.code_string and PTTrees.insert).  NanoPrelude's (<) is
-- monomorphic Int, so this hides the four Int operators and takes the class
-- from NanoOrd, which exists for exactly this case and has a ghc-shim twin,
-- so one source compiles on both versions.
module Compress where
import Prelude()
import NanoClasses hiding ((<), (<=), (>), (>=))
import NanoOrd

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

take :: Int -> [a] -> [a]
take k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : take (k-1) ys }

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

infixr 8 ^
(^) :: Int -> Int -> Int
b ^ e = if e <= 0 then 1 else b * (b ^ (e-1))

toEnum :: Int -> Char
toEnum = primChr

ord :: Char -> Int
ord = primOrd

hash :: String -> Int
hash = foldl (\acc c -> ord c + acc*31) 0

-- ===================== Defaults.hs (upstream) =====================

max_entries :: Int
max_entries = 2 ^ code_bits

first_code :: Int
first_code = 256

ascii_bits :: Int
ascii_bits = 8

code_bits :: Int
code_bits = 12

-- ===================== PTTrees.hs (upstream) ======================

data PrefixTree a b = PTNil |
                      PT (PrefixElem a b) (PrefixTree a b) (PrefixTree a b)

data PrefixElem a b = PTE a b (PrefixTree a b)

insert k v PTNil =
	PT (PTE k v PTNil) PTNil PTNil
insert k v (PT p@(PTE k' v' t) l r)
	| k < k'  = PT p (insert k v l) r
        | k > k'  = PT p l (insert k v r)
        | otherwise = PT p l r

-- ===================== BinConv.hs (upstream) ======================

zeroes = '0' : zeroes

dec_to_binx :: Int -> Int -> String
dec_to_binx x y
     = take (x - length bin_string) zeroes ++ bin_string
       where
       bin_string = dec_to_bin y

dec_to_bin = reverse . dec_to_bin'

dec_to_bin' 0 = []
dec_to_bin' x
      = (if (x `rem` 2) == 1
         then '1'
         else '0') : dec_to_bin' (x `div` 2)

codes_to_ascii :: [Int] -> [Int]
codes_to_ascii [] = []
codes_to_ascii (x:y:ns)
	= x_div : ((x_rem * 16) + y_div) : y_rem : codes_to_ascii ns
          where
          (x_div, x_rem) = divRem x 16
          (y_div, y_rem) = divRem y 256
codes_to_ascii [n]
	= [x_div , x_rem]
          where
          (x_div, x_rem) = divRem n 16

divRem x y = (x `div` y, x `rem` y)

-- ====================== Encode.hs (upstream) ======================

type CodeTable = PrefixTree Char Int

encode :: String -> [Int]
encode input = encode' input first_code initial_table

encode' [] _ _
  = []
encode' input v t
  = case (code_string input 0 v t) of { (input', n, t') ->
      n : encode' input' (v + 1) t'
    }

code_string input@(c : input2) old_code next_code (PT p@(PTE k v t) l r)
   | c < k = (f1 r1 p r)
   | c > k = (f2 r2 p l)
   | otherwise = (f3 r3 k v l r)
 where
   r1 = code_string input old_code next_code l
   r2 = code_string input old_code next_code r
   r3 = code_string input2 v next_code t

   f1 (input_l,nl,l2) p r   = (input_l,nl,PT p l2 r)
   f2 (input_r,nr,r2) p l   = (input_r,nr,PT p l r2)
   f3 (input2,n,t2) k v l r = (input2, n, PT (PTE k v t2) l r)

code_string input@(c : input_file2) old_code next_code PTNil
  | next_code >= 4096 = (input, old_code, PTNil)
  | otherwise = (input, old_code, PT (PTE c next_code PTNil) PTNil PTNil)

code_string [] old_code next_code code_table
  = ([], old_code, PTNil)

initial_table :: CodeTable
initial_table = foldr tab_insert PTNil balanced_list

tab_insert n = insert (toEnum n) n

balanced_list
    = [128,64,32,16,8,4,2,1,0,3,6,5,7,12,10,9,11,14,13,15,24,20,18,17,19,22,
       21,23,28,26,25,27,30,29,31,48,40,36,34,33,35,38,37,39,44,42,41,43,46,
       45,47,56,52,50,49,51,54,53,55,60,58,57,59,62,61,63,96,80,72,68,66,65]
      ++ bal_list2 ++ bal_list3 ++ bal_list4 ++ bal_list5

bal_list2
    = [67,70,69,71,76,74,73,75,78,77,79,88,84,82,81,83,86,85,87,92,90,89,91,
       94,93,95,112,104,100,98,97,99,102,101,103,108,106,105,107,110,109,111,
       120,116,114,113,115,118,117,119,124,122,121,123,126,125,127,192,160]

bal_list3
    = [144,136,132,130,129,131,134,133,135,140,138,137,139,142,141,143,152,
       148,146,145,147,150,149,151,156,154,153,155,158,157,159,176,168,164,
       162,161,163,166,165,167,172,170,169,171,174,173,175,184,180,178,177]

bal_list4
    = [179,182,181,183,188,186,185,187,190,189,191,224,208,200,196,194,193,
       195,198,197,199,204,202,201,203,206,205,207,216,212,210,209,211,214,
       213,215,220,218,217,219,222,221,223,240,232,228,226,225,227,230,229,
       231,236,234,233,235,238,237,239,248,244,242,241,243,246,245,247,252]
bal_list5
    = [250,249,251,254,253,255]

-- ======================= Main.hs (upstream) =======================

compress :: String -> String
compress = map toEnum . codes_to_ascii . encode

fastInput :: String
fastInput = "<mediawiki xmlns=\"http://www.mediawiki.org/xml/export-0.3/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.mediawiki.org/xml/export-0.3/ http://www.mediawiki.org/xml/export-0.3.xsd\" version=\"0.3\" xml:lang=\"en\">\n  <siteinfo>\n    <sitename>Wikipedia</sitename>\n    <base>http://en.wikipedia.org/wiki/Main_Page</base>\n    <generator>MediaWiki 1.6alpha</generator>\n    <case>first-letter</case>\n      <namespaces>\n      <namespace key=\"-2\">Media</namespace>\n      <namespace key=\"-1\">Special</namespace>\n      <namespace key=\"0\" />\n      <namespace key=\"1\">Talk</namespace>\n      <namespace key=\"2\">User</namespace>\n      <namespace key=\"3\">User talk</namespace>\n      <namespace key=\"4\">Wikipedia</namespace>\n      <namespace key=\"5\">Wikipedia talk</namespace>\n      <namespace key=\"6\">Image</namespace>\n      <namespace key=\"7\">Image talk</namespace>\n      <namespace key=\"8\">MediaWiki</namespace>\n      <namespace key=\"9\">MediaWiki talk</namespace>\n      <namespace key=\"10\">Template</namespace>\n      <namespace key=\"11\">Template talk</namespace>\n      <namespace key=\"12\">Help</namespace>\n      <namespace key=\"13\">Help talk</namespace>\n      <namespace key=\"14\">Category</namespace>\n      <namespace key=\"15\">Category talk</namespace>\n      <namespace key=\"100\">Portal</namespace>\n      <namespace key=\"101\">Portal talk</namespace>\n    </namespaces>\n  </siteinfo>\n  <page>\n    <title>AaA</title>\n    <id>1</id>\n    <revision>\n      <id>32899315</id>\n      <timestamp>2005-12-27T18:46:47Z</timestamp>\n      <contributor>\n        <username>Jsmethers</username>\n        <id>614213</id>\n      </contributor>\n      <text xml:space=\"preserve\">#REDIRECT [[AAA]]</text>\n    </revision>\n  </page>\n  <page>\n    <title>AlgeriA</title>\n    <id>5</id>\n    <revision>\n      <id>18063769</id>\n      <timestamp>2005-07-03T11:13:13Z</timestamp>\n      <contributor>\n        <username>Docu</username>\n        <id>8029</id>\n"

fastN, simN :: Int
fastN = 200
simN  = 1

-- upstream shortens the input by (n `mod` 31) on repetition n; at simN = 1
-- that is n = 1, so the run compresses the first len-1 bytes.
bench :: Int
bench = hash (compress (take (length fastInput - 1) fastInput))

main :: Int
main = bench
