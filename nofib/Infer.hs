-- Infer.hs - nofib real/infer (Hindley-Milner type inference; Jones' modular
-- monadic reformulation), NanoClasses dialect.  THE BENCHMARK BODY IS
-- UPSTREAM'S, UNTOUCHED: everything below the glue block is Shows.hs,
-- Parse.hs, MyList.hs, Type.hs, Term.hs, FiniteMap.hs, Substitution.hs,
-- Environment.hs, MaybeM.hs, StateX.hs, InferMonad.hs, Infer.hs and Main.hs
-- concatenated, minus their module headers and imports.  The only edit the
-- flattening forces: four modules each define a private `rep`, so they become
-- repSub/repEnv/repSX/repI.  Nothing else is touched.
--
-- Upstream main: replicateM_ 200 (print (hash (showsString (show testEnv ++
-- prompt) inferred))) over infer.faststdin (348 bytes, 16 terms).
-- SIM SCALE (same ruling as Grep): fastN = 200 repetitions; simN = 1.
--
-- Two things the dialect has to supply.  The Char predicates Parse.hs takes
-- from Data.Char are in the glue, as in Grep.  And the body says `reads` for
-- four different types, so it needs a Read class: NanoRead provides it with a
-- ghc-shim twin, the same shape as NanoOrd.  `read` is defined in NanoRead
-- identically on BOTH versions rather than re-exported from Prelude, so the two
-- backends cannot disagree about what a complete parse is.
module Infer where
import Prelude()
import NanoClasses hiding ((++), head, tail, elem, and, nub, lines,
                           unwords, error, read, reads)
import NanoRead
import Primitives (primCharLE, primCharEQ)

-- ------------------------------- glue --------------------------------

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

elem :: Eq a => a -> [a] -> Bool
elem x = any (\y -> y == x)

and :: [Bool] -> Bool
and = all id

nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (\y -> not (y == x)) xs)

lines :: String -> [String]
lines s = case breakNl s of
            (l, s') -> l : case s' of { [] -> []; (_:rest) -> lines rest }
  where
    breakNl []     = ([], [])
    breakNl (c:cs) = if primCharEQ c (primChr 10) then ([], c:cs)
                     else case breakNl cs of { (a,b) -> (c:a, b) }

unwords :: [String] -> String
unwords []     = []
unwords [w]    = w
unwords (w:ws) = w ++ (primChr 32 : unwords ws)

-- Char classification, via primOrd and an Int comparison: same as Grep.
isSpace :: Char -> Bool
isSpace c = primCharEQ c (primChr 32) || primCharEQ c (primChr 9)
         || primCharEQ c (primChr 10) || primCharEQ c (primChr 13)
         || primCharEQ c (primChr 12) || primCharEQ c (primChr 11)
isUpper c = primCharLE (primChr 65) c && primCharLE c (primChr 90)
isLower c = primCharLE (primChr 97) c && primCharLE c (primChr 122)
isDigit c = primCharLE (primChr 48) c && primCharLE c (primChr 57)
isAlpha c = isUpper c || isLower c
isAlphaNum c = isAlpha c || isDigit c
isAscii c = primCharLE c (primChr 127)
isControl c = primCharLE c (primChr 31) || primCharEQ c (primChr 127)
isPrint c = primCharLE (primChr 32) c && primCharLE c (primChr 126)

-- A bottom the benchmark never reaches: FiniteMap.lookupFM passes `error` as
-- the default for a key that a well-typed run always finds.
bottom :: a
bottom = bottom

error :: String -> a
error _ = bottom

hash :: String -> Int
hash = foldl (\acc c -> primOrd c + acc*31) 0

-- =========================== Shows.hs =============================

type  Shows x  =  x -> ShowS
showsEmpty                    :: ShowS
showsEmpty r                  =  r
showsConcat                   :: [ShowS] -> ShowS
showsConcat                   =  foldr (.) showsEmpty
showsString                   :: Shows String
showsString                   =  (++)
showsChar                     :: Shows Char
showsChar                     =  (:)
showsStar                     :: Shows x -> Shows [x]
showsStar showsX xs           =  showsConcat (map showsX xs)
showsStarSep                  :: String -> Shows x -> Shows [x]
showsStarSep s showsX []      =  showsEmpty
showsStarSep s showsX (x:xs)  =  showsX x
                              .  showsConcat [showString s . showsX x' | x' <- xs]
showsSurround                 :: String -> Shows x -> String -> Shows x
showsSurround l showsX r x    =  showString l . showsX x . showString r
showsListOf                   :: Shows x -> Shows [x]
showsListOf showsX            =  showsSurround "[" (showsStarSep ", " showsX) "]"
showsParen                   :: ShowS -> ShowS
showsParen                   =  showsSurround "(" id ")"
showsParenIf                  :: Bool -> ShowS -> ShowS
showsParenIf b xS             =  if  b  then  showsParen xS  else  xS

-- =========================== Parse.hs =============================

infixr 1      `elseP`
infix  2      `thenP`
infix  2      `eachP`
infixr 3      `filterP`
infixr 3      `guardP`
type Parse a x  =  a -> [(x, a)]
type Parses x  =  Parse String x
thenP         :: Parse a x -> (x -> Parse a y) -> Parse a y
xP `thenP` kP =  \a -> [ (y,c) | (x,b) <- xP a, (y,c) <- kP x b ]
returnP       :: x -> Parse a x
returnP x     =  \a -> [ (x,a) ]
eachP         :: Parse a x -> (x -> y) -> Parse a y
xP `eachP` f  =  xP `thenP` (\x -> returnP (f x))
consP           :: Parse a x -> Parse a [x] -> Parse a [x]
xP `consP` xsP  =  xP   `thenP` (\x ->
                   xsP  `thenP` (\xs ->
                        returnP (x:xs)))
elseP         :: Parse a x -> Parse a x -> Parse a x
xP `elseP` yP =  \a -> xP a ++ yP a
failP         :: Parse a x
failP         =  \a -> []
guardP        :: Bool -> Parse a x -> Parse a x
guardP b xP   =  if  b  then  xP  else  failP
filterP       :: (x -> Bool) -> Parse a x -> Parse a x
filterP p xP  =  xP `thenP` (\x -> p x `guardP` returnP x)
starP         :: Parse a x -> Parse a [x]
starP xP      =  cutP (plusP xP `elseP` returnP [])
plusP         :: Parse a x -> Parse a [x]
plusP xP      =  xP `consP` starP xP
cutP          :: Parse a x -> Parse a x
cutP xP       =  \a -> case  xP a  of  { ~(~(x,b):_) -> [(x,b)] }
endP          :: Parse [x] ()
endP          =  \xs -> if  null xs  then  returnP () xs  else  failP xs
itemP         :: Parse [x] x
itemP         =  \xs -> if  null xs  then  failP xs
                                     else  returnP (head xs) (tail xs)
litP          :: (Eq x) => x -> Parse [x] x
litP c        =  (\x -> c==x) `filterP` itemP
litsP         :: (Eq x) => [x] -> Parse [x] [x]
litsP []      =  returnP []
litsP (c:cs)  =  litP c `consP` litsP cs
exactlyP      :: Parse [y] x -> Parse [y] x
exactlyP xP   =  xP `thenP` (\x -> endP `thenP` (\() -> returnP x))
spacesP       :: Parses String
spacesP       =  starP spaceP
lexicalP      :: Parses x -> Parses x
lexicalP xP   =  xP `thenP` (\x -> spacesP `thenP` (\_ -> returnP x))
lexP          :: String -> Parses String
lexP cs       =  lexicalP (litsP cs)
lexactlyP     :: Parses x -> Parses x
lexactlyP xP  =  spacesP `thenP` (\_ -> exactlyP xP)
asciiP, controlP, printP, spaceP, upperP      :: Parses Char
lowerP, alphaP, digitP, alphanumP             :: Parses Char
asciiP        =  isAscii    `filterP` itemP
controlP      =  isControl  `filterP` itemP
printP        =  isPrint    `filterP` itemP
spaceP        =  isSpace    `filterP` itemP
upperP        =  isUpper    `filterP` itemP
lowerP        =  isLower    `filterP` itemP
alphaP        =  isAlpha    `filterP` itemP
digitP        =  isDigit    `filterP` itemP
alphanumP     =  isAlphaNum `filterP` itemP
surroundP             :: String -> Parses x -> String -> Parses x
surroundP l xP r      =  lexP l       `thenP` (\_ ->
                         xP           `thenP` (\x ->
                         lexP r       `thenP` (\_ ->
                                      returnP x)))
plusSepP              :: String -> Parses x -> Parses [x]
plusSepP s xP         =  xP `consP` starP (lexP s `thenP` (\_ -> xP))
starSepP              :: String -> Parses x -> Parses [x]
starSepP s xP         =  plusSepP s xP `elseP` returnP []
parenP                :: Parses x -> Parses x
parenP xP             =  surroundP "(" xP ")"
listP                 :: Parses x -> Parses [x]
listP xP              =  surroundP "[" (starSepP "," xP) "]"
useP          :: x -> Parse a x -> (a -> x)
useP failx xP =  \a -> case  xP a  of { [] -> failx; ((x,_):_) -> x }

-- =========================== MyList.hs ============================

minus                 :: (Eq x) => [x] -> [x] -> [x]
xs `minus` ys         =  foldl rmv xs ys
rmv                   :: (Eq x) => [x] -> x -> [x]
[] `rmv` y            =  []
(x:xs) `rmv` y        =  if  x == y  then  xs  else  x : (xs `rmv` y)

-- ============================ Type.hs =============================

type  TVarId          =  String
type  TConId          =  String
data  MonoType        =  TVar TVarId
                      |  TCon TConId [MonoType]

data  PolyType        =  All [TVarId] MonoType
u `arrow` v           =  TCon "->" [u,v]
freeTVarMono                  :: MonoType -> [TVarId]
freeTVarMono (TVar x)         =  [x]
freeTVarMono (TCon k ts)      =  concat (map freeTVarMono ts)
freeTVarPoly                  :: PolyType -> [TVarId]
freeTVarPoly (All xs t)       =  nub (freeTVarMono t) `minus` xs

instance Eq MonoType where
    (TVar tv1)       == (TVar tv2)	 = tv1 == tv2
    (TCon tc1 args1) == (TCon tc2 args2) = tc1 == tc2 && (args1 == args2)
    other1	     == other2		 = False

instance  Read MonoType  where
      readsPrec d     =  readsMono d
instance  Show MonoType  where
      showsPrec d     =  showsMono d

readsMono             :: Int -> Parses MonoType
readsMono d           =       ((d<=1) `guardP` readsArrow)
                      `elseP` ((d<=9) `guardP` readsTCon)
                      `elseP` (readsTVar)
                      `elseP` (parenP (readsMono 0))

readsArrow            :: Parses MonoType
readsArrow            =  readsMono 2          `thenP` (\u ->
                         lexP "->"            `thenP` (\_ ->
                         readsMono 1          `thenP` (\v ->
                                              returnP (u `arrow` v))))
readsTCon             :: Parses MonoType
readsTCon             =  readsTConId          `thenP` (\k  ->
                         starP (readsMono 10) `thenP` (\ts ->
                                              returnP (TCon k ts)))
readsTVar             :: Parses MonoType
readsTVar             =  readsTVarId          `thenP` (\x ->
                                              returnP (TVar x))
readsTVarId           :: Parses String
readsTVarId           =  lexicalP (lowerP `consP` starP alphaP)
readsTConId           :: Parses String
readsTConId           =  lexicalP (upperP `consP` starP alphaP)
showsMono             :: Int -> Shows MonoType
showsMono d (TVar xx)
      =  showsString xx
showsMono d (TCon "->" [uu,vv])
      =  showsParenIf (d>1)
         (showsMono 2 uu . showsString " -> " . showsMono 1 vv)
showsMono d (TCon kk tts)
      =  showsParenIf (d>9)
         (showsString kk .
          showsStar (\tt -> showsString " " . showsMono 10 tt) tts)
instance  Read PolyType  where
      readsPrec d             =  reads `eachP` polyFromMono
instance  Show PolyType  where
      showsPrec d (All xs t)  =  showsString "All " . showsString (unwords xs) .
                                 showsString ". " . showsMono 0 t
polyFromMono          :: MonoType -> PolyType
polyFromMono t        =  All (nub (freeTVarMono t)) t

-- ============================ Term.hs =============================

type  VarId   =  String
data  Term    =  Var VarId
              |  Abs VarId Term
              |  App Term Term
              |  Let VarId Term Term
instance Show Term where
      showsPrec d  =  showsTerm d
instance Read Term where
      readsPrec d  =  readsTerm
readsTerm, readsAbs, readsAtomics, readsAtomic, readsVar :: Parses Term
readsTerm     =       readsAbs
              `elseP` readsLet
              `elseP` readsAtomics
readsAtomic   =       readsVar
              `elseP` parenP readsTerm
readsAbs      =       lexP "\\"               `thenP` (\_  ->
                      plusP readsId           `thenP` (\xs ->
                      lexP "."                `thenP` (\_  ->
                      readsTerm               `thenP` (\v  ->
                                              returnP (foldr Abs v xs)))))
readsLet      =       lexP "let"              `thenP` (\_ ->
                      readsId                 `thenP` (\x ->
                      lexP "="                `thenP` (\_ ->
                      readsTerm               `thenP` (\u ->
                      lexP "in"               `thenP` (\_ ->
                      readsTerm               `thenP` (\v ->
                                              returnP (Let x u v)))))))
readsAtomics  =       readsAtomic             `thenP` (\t  ->
                      starP readsAtomic       `thenP` (\ts ->
                                              returnP (foldl App t ts)))
readsVar      =       readsId                 `thenP` (\x ->
                                              returnP (Var x))
readsId       :: Parses String
readsId       =  lexicalP (isntKeyword `filterP` plusP alphaP)
                 where  isntKeyword x  =  (x /= "let" && x /= "in")
showsTerm                     :: Int -> Shows Term
showsTerm d (Var x)           =  showsString x
showsTerm d (Abs x v)         =  showsParenIf (d>0)
                                 (showsString "\\" . showsString x . showsAbs v)
showsTerm d (App t u)         =  showsParenIf (d>1)
                                 (showsTerm 1 t . showsChar ' ' . showsTerm 2 u)
showsTerm d (Let x u v)       =  showsParenIf (d>0)
                                 (showsString "let  "   . showsString x .
                                  showsString " = "     . showsTerm 1 u .
                                  showsString "  in  "  . showsTerm 0 v)
showsAbs                      :: Shows Term
showsAbs (Abs x t)            =  showsString " " . showsString x . showsAbs t
showsAbs t                    =  showsString ". " . showsTerm 0 t

-- ========================== FiniteMap.hs ==========================

data  FM a b  =  MkFM [(a,b)]
emptyFM                               ::  FM a b
emptyFM                               =   MkFM []
unitFM                                ::  a -> b -> FM a b
unitFM a b                            =   MkFM [(a,b)]
extendFM                              ::  FM a b -> a -> b -> FM a b
extendFM (MkFM abs) a b               =   MkFM ((a,b) : abs)
makeFM                                ::  [(a,b)] -> FM a b
makeFM abs                            =   MkFM abs
unmakeFM                              ::  FM a b -> [(a,b)]
unmakeFM (MkFM abs)                   =   abs
thenFM                                ::  FM a b -> FM a b -> FM a b
(MkFM abs1) `thenFM` (MkFM abs2)      =   MkFM (abs2 ++ abs1)
plusFM                                ::  (Eq a) => FM a b -> FM a b -> FM a b
f `plusFM` g  |  f `disjointFM` g     =   f `thenFM` g
lookupFM                              ::  (Eq a) => FM a b -> a -> b
lookupFM f a                          =   lookupElseFM (error "lookup") f a
lookupElseFM                          ::  (Eq a) => b -> FM a b -> a -> b
lookupElseFM b (MkFM abs) a           =   head (  [ b' | (a',b') <- abs, a==a' ]
                                               ++ [ b ] )
mapFM                                 ::  (b -> c) -> FM a b -> FM a c
mapFM h (MkFM abs)                    =   MkFM [ (a, h b) | (a,b) <- abs ]
domFM                                 ::  FM a b -> [a]
domFM (MkFM abs)                      =   [ a | (a,b) <- abs ]
ranFM                                 ::  FM a b -> [b]
ranFM (MkFM abs)                      =   [ b | (a,b) <- abs ]
disjointFM                            ::  (Eq a) => FM a b -> FM a b -> Bool
f `disjointFM` g                      =   domFM f `disjoint` domFM g
disjoint                              ::  (Eq a) => [a] -> [a] -> Bool
xs `disjoint` ys                      =   and [ not (x `elem` ys) | x <- xs ]

-- ========================= Substitution.hs ========================

data  Sub  =  MkSub (FM TVarId MonoType)
repSub                        ::  Sub -> FM TVarId MonoType
repSub (MkSub f)              =   f
applySub                      ::  Sub -> MonoType -> MonoType
applySub s (TVar x)           =   lookupSub s x
applySub s (TCon k ts)        =   TCon k (map (applySub s) ts)
lookupSub                     ::  Sub -> TVarId -> MonoType
lookupSub s x                 =   lookupElseFM (TVar x) (repSub s) x
unitSub                       ::  TVarId -> MonoType -> Sub
unitSub x t                   =   MkSub (makeFM [(x,t)])
emptySub                      ::  Sub
emptySub                      =   MkSub emptyFM
makeSub                       ::  [(TVarId, MonoType)] -> Sub
makeSub xts                   =   MkSub (makeFM xts)
extendSub                     ::  Sub -> TVarId -> MonoType -> Sub
extendSub s x t               =   s `thenSub` unitSub x (applySub s t)
thenSub                       ::  Sub -> Sub -> Sub
r `thenSub` s                 =   MkSub (mapFM (applySub s) (repSub r) `thenFM` repSub s)
domSub                        ::  Sub -> [TVarId]
domSub s                      =   domFM (repSub s)
unifySub                              =  unify
unify                                 :: MonoType -> MonoType -> Sub -> Maybe Sub
unify (TVar x) u s                    =  unifyTVar x u s
unify t (TVar y) s                    =  unifyTVar y t s
unify (TCon j ts) (TCon k us) s       =  (j == k) `guardM` unifies ts us s
unifies                               :: [MonoType] -> [MonoType] -> Sub -> Maybe Sub
unifies [] [] s                       =  returnM s
unifies (t:ts) (u:us) s               =  unify t u s `thenM` (\s' -> unifies ts us s')
unifyTVar                             :: TVarId -> MonoType -> Sub -> Maybe Sub
unifyTVar x t s | x `elem` domSub s     =  unify (lookupSub s x) t s
                | TVar x == t         =  returnM s
                | x `elem` freeVars t   =  failM
                | otherwise           =  returnM (extendSub s x t)
freeVars                              =  freeTVarMono

-- ========================= Environment.hs =========================

data  Env             =   MkEnv (FM VarId PolyType)
repEnv                ::  Env -> FM VarId PolyType
repEnv (MkEnv f)      =   f
emptyEnv              ::  Env
emptyEnv              =   MkEnv emptyFM
extendLocal           ::  Env -> VarId -> MonoType -> Env
extendLocal env x t   =   MkEnv (extendFM (repEnv env) x (All [] t))
extendGlobal          ::  Env -> VarId -> PolyType -> Env
extendGlobal env x t  =   MkEnv (extendFM (repEnv env) x t)
makeEnv               ::  [(VarId, PolyType)] -> Env
makeEnv               =   MkEnv . makeFM
unmakeEnv             ::  Env -> [(VarId, PolyType)]
unmakeEnv             =   unmakeFM . repEnv
lookupEnv             ::  Env -> VarId -> PolyType
lookupEnv env x       =   lookupFM (repEnv env) x
domEnv                ::  Env -> [VarId]
domEnv env            =   domFM (repEnv env)
freeTVarEnv           ::  Env -> [TVarId]
freeTVarEnv env       =   concat (map freeTVarPoly (ranFM (repEnv env)))
instance  Read Env  where
      readsPrec d  =  readsEnv
instance  Show Env  where
      showsPrec d  =  showsEnv
readsEnv              :: Parses Env
readsEnv              =  listP readsPair `eachP` makeEnv
readsPair             :: Parses (VarId, PolyType)
readsPair             =       readsId         `thenP` (\x ->
                              lexP ":"        `thenP` (\_ ->
                              reads           `thenP` (\t ->
                                              returnP (x,t))))
showsEnv              :: Shows Env
showsEnv              =  showsSurround "[" (showsStarSep ",\n " showsPair) "]"
                      .  unmakeEnv
showsPair             :: Shows (VarId, PolyType)
showsPair (x,t)       =  showsString x . showsString " : " . shows t

-- =========================== MaybeM.hs ============================

returnM               ::  x -> Maybe x
returnM x             =   Just x
eachM                 ::  Maybe x -> (x -> y) -> Maybe y
(Just x) `eachM` f    =   Just (f x)
Nothing `eachM` f     =   Nothing
thenM                 ::  Maybe x -> (x -> Maybe y) -> Maybe y
(Just x) `thenM` kM   =   kM x
Nothing `thenM` kM    =   Nothing
failM                 ::  Maybe x
failM                 =   Nothing
orM                   ::  Maybe x -> Maybe x -> Maybe x
(Just x) `orM` yM     =   Just x
Nothing `orM` yM      =   yM
guardM                ::  Bool -> Maybe x -> Maybe x
b `guardM` xM         =   if  b  then  xM  else  failM
filterM               ::  (x -> Bool) -> Maybe x -> Maybe x
p `filterM` xM        =   xM `thenM` (\x -> p x `guardM` returnM x)
theM                  ::  Maybe x -> x
theM (Just x)         =   x
existsM               ::  Maybe x -> Bool
existsM (Just x)      =   True
existsM Nothing       =   False
useM                  ::  x -> Maybe x -> x
useM xfail (Just x)   =   x
useM xfail Nothing    =   xfail

-- =========================== StateX.hs ============================

data  StateX s a      =   MkSX (s -> a)
repSX (MkSX f)        =   f
returnSX returnX x    =   MkSX (\s -> returnX (x, s))
eachSX eachX xSX f    =   MkSX (\s -> repSX xSX s `eachX` (\(x,s') -> (f x, s')))
thenSX thenX xSX kSX  =   MkSX (\s -> repSX xSX s `thenX` (\(x,s') -> repSX (kSX x) s'))
toSX eachX xX         =   MkSX (\s -> xX `eachX` (\x -> (x,s)))
putSX returnX s'      =   MkSX (\s -> returnX ((), s'))
getSX returnX         =   MkSX (\s -> returnX (s,s))
useSX eachX s xSX     =   repSX xSX s `eachX` (\(x,s') -> x)

-- ========================= InferMonad.hs ==========================

type  Counter         =  Int
data  Infer x         =  MkI (StateX Sub (StateX Counter (Maybe ((x, Sub), Counter))))
repI (MkI xJ)         =  xJ
returnI               :: x -> Infer x
returnI x             =  MkI (returnSX (returnSX returnM) x)
eachI                 :: Infer x -> (x -> y) -> Infer y
xI `eachI` f          =  MkI (eachSX (eachSX eachM) (repI xI) f)
thenI                 :: Infer x -> (x -> Infer y) -> Infer y
xI `thenI` kI         =  MkI (thenSX (thenSX thenM) (repI xI) (repI . kI))
failI                 :: Infer x
failI                 =  MkI (toSX (eachSX eachM) (toSX eachM failM))
useI                  :: x -> Infer x -> x
useI xfail            =  useM xfail
                      .  useSX eachM 0
                      .  useSX (eachSX eachM) emptySub
                      .  repI
guardI                :: Bool -> Infer x -> Infer x
guardI b xI           =  if  b  then  xI  else  failI
putSubI               :: Sub -> Infer ()
putSubI s             =  MkI (putSX (returnSX returnM) s)
getSubI               :: Infer Sub
getSubI               =  MkI (getSX (returnSX returnM))
putCounterI           :: Counter -> Infer ()
putCounterI c         =  MkI (toSX (eachSX eachM) (putSX returnM c))
getCounterI           :: Infer Counter
getCounterI           =  MkI (toSX (eachSX eachM) (getSX returnM))
substituteI           :: MonoType -> Infer MonoType
substituteI t         =  getSubI              `thenI`  (\ s ->
                                              returnI  (applySub s t))
unifyI                :: MonoType -> MonoType -> Infer ()
unifyI t u            =  getSubI              `thenI`  (\ s  ->
			 let sM = unifySub t u s
			 in
                         existsM sM           `guardI` (
                         putSubI (theM sM)    `thenI`  (\ () ->
                                              returnI  ())))
freshI                :: Infer MonoType
freshI                =  getCounterI          `thenI` (\c  ->
                         putCounterI (c+1)    `thenI` (\() ->
                                              returnI (TVar ("a" ++ show c))))
freshesI              :: Int -> Infer [MonoType]
freshesI 0            =                       returnI []
freshesI n            =  freshI               `thenI` (\x  ->
                         freshesI (n-1)       `thenI` (\xs ->
                                              returnI (x:xs)))

-- =========================== Infer.hs =============================

specialiseI                   :: PolyType -> Infer MonoType
specialiseI (All xxs tt)      =  freshesI (length xxs) `thenI` (\yys ->
                                 returnI (applySubs xxs yys tt))
applySubs                     :: [TVarId] -> [MonoType] -> MonoType -> MonoType
applySubs xxs yys tt          =  applySub (makeSub (zip xxs yys)) tt
generaliseI                   :: Env -> MonoType -> Infer PolyType
generaliseI aa tt             =  getSubI `thenI` (\s ->
 				 let aaVars = nub (freeTVarSubEnv s aa) in
				 let ttVars = nub (freeTVarMono tt) in
				 let xxs    = ttVars `minus` aaVars in
                                 returnI (All xxs tt)
                                 )
freeTVarSubEnv                :: Sub -> Env -> [TVarId]
freeTVarSubEnv s aa           =  concat (map (freeTVarMono . lookupSub s)
                                             (freeTVarEnv aa))
inferTerm  ::  Env -> Term -> Infer MonoType
inferTerm aa (Var x)  =
      (x `elem` domEnv aa)                      `guardI` (
      let ss = lookupEnv aa x in
      specialiseI ss                          `thenI`  (\tt ->
      substituteI tt                          `thenI`  (\uu  ->
                                              returnI  uu)))
inferTerm aa (Abs x v)  =
      freshI                                  `thenI` (\xx ->
      inferTerm (extendLocal aa x xx) v       `thenI` (\vv ->
      substituteI xx                          `thenI` (\uu ->
                                              returnI (uu `arrow` vv))))
inferTerm aa (App t u)  =
      inferTerm aa t                          `thenI` (\tt ->
      inferTerm aa u                          `thenI` (\uu ->
      freshI                                  `thenI` (\xx ->
      unifyI tt (uu `arrow` xx)               `thenI` (\() ->
      substituteI xx                          `thenI` (\vv ->
                                              returnI vv)))))
inferTerm aa (Let x u v)  =
      inferTerm aa u                          `thenI` (\uu ->
      generaliseI aa uu                       `thenI` (\ss ->
      inferTerm (extendGlobal aa x ss) v      `thenI` (\vv ->
                                              returnI vv)))

-- ============================ Main.hs =============================

readInferShow :: String -> String
readInferShow =  useP ("Failed to parse" ++ prompt)   (
                 lexactlyP reads                      `eachP` (\t ->
                 useI ("Failed to type" ++ prompt)    (
                 inferTerm testEnv t                  `eachI` (\tt ->
                 show t ++ " : " ++ show tt ++ prompt))))
testEnv       :: Env
testEnv       =  read
                   (   "[ unit   : x -> List x,"
                   ++  "  append : List x -> List x -> List x,"
                   ++  "  fix    : (x -> x) -> x ]"
                   )
prompt        :: String
prompt        =  "\n? "

fastInput :: String
fastInput = "(\\x.x)\n\\x.\\y.x\n(x\nx\n(\\f.\\g.\\x.(g(f(x))))\n(\\x. x x)    \n(\\x.x) (\\x.x)\n(\\id. id id) (\\x.x)\nlet id = \\x.x in id id \n\\x. let f = x in f f      \nfix (\\x.x)\nfix unit\n\\x. unit (unit x)\nfix (\\x. append (unit x))\n\\x. fix (\\xs. append (unit x) xs)\nlet id = \\x.x in id id id id id id id id id id id id id id id id id id id id id id id id id id id id id id id\n"

fastN, simN :: Int
fastN = 200
simN  = 1

bench :: Int
bench = hash (showsString (show testEnv ++ prompt)
                          (concat (map readInferShow (lines fastInput))))

main :: Int
main = bench
