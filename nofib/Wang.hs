-- Wang.hs - nofib spectral/hartel/wang (Hartel benchmark
-- suite: polymorphic type checker), NanoClasses dialect. THE BENCHMARK
-- BODY IS THE MACHINE-TRANSLATED ORIGINAL, UNTOUCHED apart from the
-- wrapper: the Fast2haskell shims it uses (fix, miraseq, pair) are the
-- standard definitions, miraseq through the machine's real seq.
-- FAST input: 8. Result: hash of the output string (port convention).
module Wang where
import Prelude()
import NanoClasses hiding (head, init, last, length, replicate, reverse, tail, take, zip, (!!), (++))
import Primitives (primCharLE, primCharEQ)

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

fix :: (x -> x) -> x
fix f = fix_f where fix_f = f fix_f

miraseq :: x -> y -> y
miraseq x y = seq x y

pair :: [x] -> Bool
pair [] = False
pair x = True

strict_show_i :: Int -> [Char]
strict_show_i x = miraseq x (show x)

ord :: Char -> Int
ord = primOrd


negD :: FloatW -> FloatW
negD = negateD

-- transcendentals are not primitives on the fun backend (X2n1 ruling):
-- fixed truncated series, identical op sequence on both backends
expT :: FloatW -> FloatW
expT x = go 1 (fromIntD 1) (fromIntD 1)
  where go k term acc = if k > 14 then acc
                        else let t = term *. x /. fromIntD k
                             in go (k + 1) t (acc +. t)

atanT :: FloatW -> FloatW
atanT x = if x >. fromIntD 1 then piD /. fromIntD 2 -. atanT (fromIntD 1 /. x)
          else if x <. negateD (fromIntD 1)
               then negateD (piD /. fromIntD 2) -. atanT (fromIntD 1 /. x)
          else go 0 x x
  where go k term acc = if k > 12 then acc
                        else let t = negateD (term *. x *. x)
                                 k2 = 2 * k + 3
                             in go (k + 1) t (acc +. t /. fromIntD k2)

piD :: FloatW
piD = fromIntD 3 +. fromIntD 14159265 /. fromIntD 100000000


length :: [a] -> Int
length = go 0 where go n [] = n
                    go n (_:xs) = go (n + 1) xs

replicate :: Int -> a -> [a]
replicate n x = if n <= 0 then [] else x : replicate (n - 1) x

-- deterministic 6-decimal float rendering (no shortest-repr float show
-- on the fun backend; self-consistent, which is all the checksum needs)
strict_show_d :: FloatW -> [Char]
strict_show_d x = miraseq x (showD x)
  where showD v = if v <. fromIntD 0 then '-' : showD (negateD v)
                  else let i  = truncateD v
                           fr = v -. fromIntD i
                           d6 = truncateD (fr *. fromIntD 1000000)
                           ds = show d6
                       in show i ++ ('.' : (replicate (6 - length ds) '0' ++ ds))


fromEnum :: Char -> Int
fromEnum = primOrd

toEnum :: Int -> Char
toEnum = primChr


f_strict :: (a -> b) -> a -> b
f_strict f x = seq x (f x)

iff :: Bool -> x -> x -> x
iff b x y = if b then x else y

abortstr :: [Char] -> a
abortstr s = abortstr s

entier :: FloatW -> FloatW
entier x = fromIntD (truncateD x)


infixl 9 !!
(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n - 1)

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

take :: Int -> [a] -> [a]
take k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : take (k-1) ys }

drop :: Int -> [a] -> [a]
drop k xs = if k <= 0 then xs else case xs of { [] -> []; (_:ys) -> drop (k-1) ys }

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs

init :: [a] -> [a]
init (_:[]) = []
init (x:xs) = x : init xs

hash :: String -> Int
hash = foldl (\acc c -> ord c + acc*31) 0



type
    T_tuple=(FloatW,FloatW,FloatW,FloatW);
    f_el21::[T_tuple] -> [T_tuple];
    f_el21 (a_row:[])=(:) a_row [];
    f_el21 ((a_a,a_b,a_c,a_r):(a_e,a_f,a_g,a_s):a_rest)=
        let {
            r_e'=negateD ((*.) r_fact a_a);
            r_f'=(-.) a_f ((*.) r_fact a_c);
            r_s'=(-.) a_s ((*.) r_fact a_r);
            r_fact=(/.) a_e a_b
         } in  (:) (a_a,a_b,a_c,a_r) (f_el21 ((:) (r_e',r_f',a_g,r_s') a_rest));
    f_el22::[T_tuple] -> [T_tuple];
    f_el22 (a_a:a_b:[])=(:) a_a ((:) a_b []);
    f_el22 ((a_a,a_b,a_c,a_r):a_row:a_rest)=
        let {
            r_a'=(-.) a_a ((*.) r_fact r_e);
            r_g'=negateD ((*.) r_fact r_g);
            r_s'=(-.) a_r ((*.) r_fact r_s);
            (r_e,r_f,r_g,r_s)=head r_res;
            r_res=f_el22 ((:) a_row a_rest);
            r_fact=(/.) a_c r_f
         } in  (:) (r_a',a_b,r_g',r_s') r_res;
    f_el23::[T_tuple] -> [T_tuple];
    f_el23 (a_a:[])=(:) a_a [];
    f_el23 ((a_a,a_b,a_c,a_r):a_row:a_rest)=
        let {
            r_g'=(-.) a_r ((*.) r_fact r_s);
            (r_e,r_f,r_g,r_s)=head r_res;
            r_res=f_el23 ((:) a_row a_rest);
            r_fact=(/.) a_c r_f
         } in  (:) (a_a,a_b,(0.00000 :: FloatW),r_g') r_res;
    c_el2::[T_tuple] -> [T_tuple];
    c_el2=(.) f_el22 f_el21;
    c_eldia::[T_tuple] -> [T_tuple];
    c_eldia=(.) f_el23 f_el21;
    f_el1::(T_tuple,T_tuple) -> T_tuple;
    f_el1 ((a_a,a_b,a_c,a_r),(a_e,a_f,a_g,a_s))=
        let {
            r_f'=(-.) a_f ((*.) r_fact a_a);
            r_g'=negateD ((*.) r_fact a_c);
            r_s'=(-.) a_s ((*.) r_fact a_r);
            r_fact=(/.) a_g a_b
         } in  (a_e,r_f',r_g',r_s');
    f_el31::[T_tuple] -> T_tuple -> [FloatW];
    f_el31 [] (a_ay,a_by,a_cy,a_ry)=(:) ((/.) a_ry a_by) [];
    f_el31 ((a_e,a_f,a_g,a_s):a_m) a_brow=
        let {
            r_gy=(*.) ((/.) a_g r_by) r_ry;
            (r_ay,r_by,r_cy,r_ry)=a_brow
         } in  (:) ((/.) a_f ((-.) a_s r_gy)) (f_el31 a_m a_brow);
    f_el3::(T_tuple,[T_tuple],T_tuple) -> [FloatW];
    f_el3 (a_arow,[],(a_ay,a_by,a_cy,a_ry))=(:) ((/.) a_ry a_by) [];
    f_el3 (a_arow,((a_e,a_f,a_g,a_s):a_m),a_brow)=
        let {
            r_ex=(*.) ((/.) a_e r_bx) r_rx;
            r_gy=(*.) ((/.) a_g r_by) r_ry;
            (r_ax,r_bx,r_cx,r_rx)=a_arow;
            (r_ay,r_by,r_cy,r_ry)=a_brow
         } in  (:) ((/.) a_f ((-.) ((-.) a_s r_ex) r_gy)) (f_el3 (a_arow,a_m,a_brow));
    f_solution::[[T_tuple]] -> [[FloatW]];
    f_solution a_x=
        let {
            r_mtrx1=f_parmap c_el2 a_x;
            r_firstrows1=f_map head (tail r_mtrx1);
            r_lastrows1=f_map f_last r_mtrx1;
            r_pairs=f_zip2 r_firstrows1 (f_init r_lastrows1);
            r_lastrows3=c_eldia ((++) (f_map f_el1 r_pairs) ((:) (f_last r_lastrows1) []));
            r_mtrx2=f_map f_init r_mtrx1;
            r_mtrx3=f_dist r_lastrows3 r_mtrx2;
            r_elem1=f_el31 (head r_mtrx2) (head r_lastrows3)
         } in  (:) r_elem1 (f_parmap f_el3 r_mtrx3);
    f_dist::[t1] -> [t2] -> [(t1,t2,t1)];
    f_dist a_rs a_ms=f_zip3 a_rs a_ms (tail a_rs);
    f_parmap::(t1 -> t2) -> [t1] -> [t2];
    f_parmap a_f a_l=f_map a_f a_l;
    c_matrix1::[[T_tuple]];
    c_matrix1=
        let {
            r_row1=((1.00000 :: FloatW),(2.00000 :: FloatW),(3.00000 :: FloatW),(6.00000 :: FloatW))
         } in  (:) ((:) ((0.00000 :: FloatW),(2.00000 :: FloatW),(3.00000 :: FloatW),(5.00000 :: FloatW)) ((:) r_row1 ((:) r_row1 ((:) r_row1 []))))
            ((:) ((:) r_row1 ((:) r_row1 ((:) r_row1 ((:) r_row1 [])))) ((:) ((:) r_row1
            ((:) r_row1 ((:) r_row1 ((:) ((1.00000 :: FloatW),(2.00000 :: FloatW),(0.00000 :: FloatW),(3.00000 :: FloatW)) [])))) []));
    c_matrix2::[[T_tuple]];
    c_matrix2=f_genmat ((0.00000 :: FloatW),(3.00000 :: FloatW),(2.00000 :: FloatW),(5.00000 :: FloatW)) ((2.00000 :: FloatW),(3.00000 :: FloatW),(2.00000 :: FloatW),(7.00000 :: FloatW)) ((2.00000 :: FloatW),(3.00000 :: FloatW),(0.00000 :: FloatW),(5.00000 :: FloatW)) (5 :: Int);
    f_bigmatrix::Int -> [[T_tuple]];
    f_bigmatrix a_n=f_genmat ((0.00000 :: FloatW),(3.00000 :: FloatW),(2.00000 :: FloatW),(5.00000 :: FloatW)) ((2.00000 :: FloatW),(3.00000 :: FloatW),(2.00000 :: FloatW),(7.00000 :: FloatW)) ((2.00000 :: FloatW),(3.00000 :: FloatW),(0.00000 :: FloatW),(5.00000 :: FloatW)) a_n;
    f_genmat a_fstrow a_midrow a_lstrow a_n=
        let {
            r_fstblock=(:) a_fstrow [a_midrow|a_i<-[(1 :: Int)..a_n]];
            r_midblock=[a_midrow|a_i<-[(1 :: Int)..((+) :: (Int -> Int -> Int)) a_n (1 :: Int)]];
            r_lstblock=(++) [a_midrow|a_i<-[(1 :: Int)..a_n]] ((:) a_lstrow [])
         } in  (:) r_fstblock ((++) [r_midblock|a_i<-[(1 :: Int)..((-) :: (Int -> Int -> Int)) a_n (2 :: Int)]] ((:) r_lstblock []));
    f_show_one::FloatW -> [Char];
    f_show_one a_x=
        if ((<=.) (f_abs ((-.) a_x (1.00000 :: FloatW))) c_epsilon)
        then "1"
        else
            (strict_show_d a_x);
    c_epsilon=(0.000500000 :: FloatW);
    f_showarray::[FloatW] -> [Char];
    f_showarray a_es=(++) "[" ((++) (tail (f_concat [(:) ',' (f_show_one a_n)|a_n<-a_es])) "]");
    f_showsolution::[[FloatW]] -> [Char];
    f_showsolution a_rs=(++) "[" ((++) ((.) tail tail (f_concat [(:) ',' ((:) '\o012'
        (f_showarray a_r))|a_r<-a_rs])) "]");
    f_benchmark_main a_n=(++) (f_sumcode (f_showsolution (f_solution (f_bigmatrix a_n)))) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let {
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
    f_abs::FloatW -> FloatW;
    f_abs a_x=
        if ((<=.) a_x (0.00000 :: FloatW))
        then (negateD a_x)
        else
            a_x;
    f_and::[Bool] -> Bool;
    f_and a_xs=f_foldr (&&) True a_xs;
    f_cjustify::Int -> [Char] -> [Char];
    f_cjustify a_n a_s=
        let {
            r_margin=((-) :: (Int -> Int -> Int)) a_n (length a_s);
            r_lmargin=((quot) :: (Int -> Int -> Int)) r_margin (2 :: Int);
            r_rmargin=((-) :: (Int -> Int -> Int)) r_margin r_lmargin
         } in  (++) (f_spaces r_lmargin) ((++) a_s (f_spaces r_rmargin));
    f_concat::[[t1]] -> [t1];
    f_concat a_xs=f_foldr (++) [] a_xs;
    f_const::t1 -> t2 -> t1;
    f_const a_x a_y=a_x;
    f_digit::Char -> Bool;
    f_digit a_x=
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum '0') (fromEnum a_x))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum '9'))
        else
            False;
    f_drop::Int -> [t1] -> [t1];
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_dropwhile::(t1 -> Bool) -> [t1] -> [t1];
    f_dropwhile a_f []=[];
    f_dropwhile a_f (a_a:a_x)=
        if (a_f a_a)
        then (f_dropwhile a_f a_x)
        else
            ((:) a_a a_x);
    c_e::FloatW;
    c_e=expT (1.00000 :: FloatW);
    f_filter::(t1 -> Bool) -> [t1] -> [t1];
    f_filter a_f a_x=[a_a|a_a<-a_x,a_f a_a];
    f_foldl::(t1 -> t2 -> t1) -> t1 -> [t2] -> t1;
    f_foldl a_op a_r []=a_r;
    f_foldl a_op a_r (a_a:a_x)=
        let {
         } in  f_foldl a_op (f_strict a_op a_r a_a) a_x;
    f_foldl1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldl1 a_op (a_a:a_x)=f_foldl a_op a_a a_x;
    f_foldr::(t1 -> t2 -> t2) -> t2 -> [t1] -> t2;
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_foldr1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldr1 a_op (a_a:[])=a_a;
    f_foldr1 a_op (a_a:a_b:a_x)=a_op a_a (f_foldr1 a_op ((:) a_b a_x));
    f_fst::(t1,t2) -> t1;
    f_fst (a_a,a_b)=a_a;
    f_id::t1 -> t1;
    f_id a_x=a_x;
    f_index::[t1] -> [Int];
    f_index a_x=
        let {
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(:) a_n (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x)
         } in  f_f (0 :: Int) a_x;
    f_init::[t1] -> [t1];
    f_init (a_a:a_x)=
        if (null a_x)
        then []
        else
            ((:) a_a (f_init a_x));
    f_iterate::(t1 -> t1) -> t1 -> [t1];
    f_iterate a_f a_x=(:) a_x (f_iterate a_f (a_f a_x));
    f_last::[t1] -> t1;
    f_last a_x=(!!) a_x (((-) :: (Int -> Int -> Int)) (length a_x) (1 :: Int));
    f_lay::[[Char]] -> [Char];
    f_lay []=[];
    f_lay (a_a:a_x)=(++) a_a ((++) "\n" (f_lay a_x));
    f_layn::[[Char]] -> [Char];
    f_layn a_x=
        let {
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(++) (f_rjustify (4 :: Int) (strict_show_i a_n)) ((++) ") " ((++) a_a ((++) "\n"
                (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x))))
         } in  f_f (1 :: Int) a_x;
    f_letter::Char -> Bool;
    f_letter a_c=
        if (
            if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'a') (fromEnum a_c))
            then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'z'))
            else
                False)
        then True
        else
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'A') (fromEnum a_c))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'Z'))
        else
            False;
    f_limit::[FloatW] -> FloatW;
    f_limit (a_a:a_b:a_x)=
        if ((==.) a_a a_b)
        then a_a
        else
            (f_limit ((:) a_b a_x));
    f_lines::[Char] -> [[Char]];
    f_lines []=[];
    f_lines (a_a:a_x)=
        let {
            r_xs=
                if (pair a_x)
                then (f_lines a_x)
                else
                    ((:) [] [])
         } in
            if (((==) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum '\o012'))
            then ((:) [] (f_lines a_x))
            else
                ((:) ((:) a_a (head r_xs)) (tail r_xs));
    f_ljustify::Int -> [Char] -> [Char];
    f_ljustify a_n a_s=(++) a_s (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s)));
    f_map::(t1 -> t2) -> [t1] -> [t2];
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    f_map2::(t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3];
    f_map2 a_f a_x a_y=[a_f a_a a_b|(a_a,a_b)<-f_zip2 a_x a_y];
    f_max::[Int] -> Int;
    f_max a_xs=f_foldl1 f_max2 a_xs;
    f_max2::Int -> Int -> Int;
    f_max2 a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) a_a a_b)
        then a_a
        else
            a_b;
    f_member::[Int] -> Int -> Bool;
    f_member a_x a_a=f_or (f_map (flip ((==) :: (Int -> Int -> Bool)) a_a) a_x);
    f_merge::[Int] -> [Int] -> [Int];
    f_merge [] a_y=a_y;
    f_merge (a_a:a_x) []=(:) a_a a_x;
    f_merge (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_merge a_x ((:) a_b a_y)))
        else
            ((:) a_b (f_merge ((:) a_a a_x) a_y));
    f_min::[Int] -> Int;
    f_min a_xs=f_foldl1 f_min2 a_xs;
    f_min2::Int -> Int -> Int;
    f_min2 a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then a_b
        else
            a_a;
    f_mkset::[Int] -> [Int];
    f_mkset []=[];
    f_mkset (a_a:a_x)=(:) a_a (f_filter (flip ((/=) :: (Int -> Int -> Bool)) a_a) (f_mkset a_x));
    f_or::[Bool] -> Bool;
    f_or a_xs=f_foldr (||) False a_xs;
    c_pi::FloatW;
    c_pi=(*.) (4.00000 :: FloatW) (atanT (1.00000 :: FloatW));
    f_postfix::t1 -> [t1] -> [t1];
    f_postfix a_a a_x=(++) a_x ((:) a_a []);
    f_product::[Int] -> Int;
    f_product a_xs=f_foldl ((*) :: (Int -> Int -> Int)) (1 :: Int) a_xs;
    f_rep::Int -> t1 -> [t1];
    f_rep a_n a_x=f_take a_n (f_repeat a_x);
    f_repeat::t1 -> [t1];
    f_repeat a_x=(:) a_x (f_repeat a_x);
    f_reverse::[t1] -> [t1];
    f_reverse a_xs=f_foldl (flip (:)) [] a_xs;
    f_rjustify::Int -> [Char] -> [Char];
    f_rjustify a_n a_s=(++) (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s))) a_s;
    f_scan::(t1 -> t2 -> t1) -> t1 -> [t2] -> [t1];
    f_scan a_op=
        let {
            f_g a_r []=(:) a_r [];
            f_g a_r (a_a:a_x)=(:) a_r (f_g (a_op a_r a_a) a_x)
         } in  f_g;
    f_snd::(t1,t2) -> t2;
    f_snd (a_a,a_b)=a_b;
    f_sort::[Int] -> [Int];
    f_sort a_x=
        let {
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else
                (f_merge (f_sort (f_take r_n2 a_x)) (f_sort (f_drop r_n2 a_x)));
    f_spaces::Int -> [Char];
    f_spaces a_n=f_rep a_n ' ';
    f_subtract::Int -> Int -> Int;
    f_subtract a_x a_y=((-) :: (Int -> Int -> Int)) a_y a_x;
    f_sum::[Int] -> Int;
    f_sum a_xs=f_foldl ((+) :: (Int -> Int -> Int)) (0 :: Int) a_xs;
data
    T_sys_message=F_Stdout [Char] | F_Stderr [Char] | F_Tofile [Char] [Char] | F_Closefile [Char] | F_Appendfile [Char] | F_System [Char] | F_Exit Int;
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_takewhile::(t1 -> Bool) -> [t1] -> [t1];
    f_takewhile a_f []=[];
    f_takewhile a_f (a_a:a_x)=
        if (a_f a_a)
        then ((:) a_a (f_takewhile a_f a_x))
        else
            [];
    f_transpose::[[t1]] -> [[t1]];
    f_transpose a_x=
        let {
            r_x'=f_takewhile pair a_x
         } in
            if (null r_x')
            then []
            else
                ((:) (f_map head r_x') (f_transpose (f_map tail r_x')));
    f_until::(t1 -> Bool) -> (t1 -> t1) -> t1 -> t1;
    f_until a_f a_g a_x=
        if (a_f a_x)
        then a_x
        else
            (f_until a_f a_g (a_g a_x));
    f_zip2::[t1] -> [t2] -> [(t1,t2)];
    f_zip2 (a_a:a_x) (a_b:a_y)=(:) (a_a,a_b) (f_zip2 a_x a_y);
    f_zip2 a_x a_y=[];
    f_zip3 (a_a:a_x) (a_b:a_y) (a_c:a_z)=(:) (a_a,a_b,a_c) (f_zip3 a_x a_y a_z);
    f_zip3 a_x a_y a_z=[];
    f_zip4 (a_a:a_w) (a_b:a_x) (a_c:a_y) (a_d:a_z)=(:) (a_a,a_b,a_c,a_d) (f_zip4 a_w a_x a_y a_z);
    f_zip4 a_w a_x a_y a_z=[];
    f_zip5 (a_a:a_v) (a_b:a_w) (a_c:a_x) (a_d:a_y) (a_e:a_z)=(:) (a_a,a_b,a_c,a_d,a_e) (f_zip5 a_v a_w a_x a_y a_z);
    f_zip5 a_v a_w a_x a_y a_z=[];
    f_zip6 (a_a:a_u) (a_b:a_v) (a_c:a_w) (a_d:a_x) (a_e:a_y) (a_f:a_z)=(:) (a_a,a_b,a_c,a_d,a_e,a_f) (f_zip6 a_u a_v a_w a_x a_y a_z);
    f_zip6 a_u a_v a_w a_x a_y a_z=[];
    f_zip::([t1],[t2]) -> [(t1,t2)];
    f_zip (a_x,a_y)=f_zip2 a_x a_y;
    f_main a_x=f_benchmark_main a_x;
bench :: Int
bench = hash (f_main 8)

main :: Int
main = bench
