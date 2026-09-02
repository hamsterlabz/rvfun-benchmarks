-- Prolog.hs - nofib real/prolog (Mark P. Jones' Mini Prolog, stack engine),
-- NanoClasses dialect. THE BENCHMARK BODY IS UPSTREAM'S, UNTOUCHED: the
-- declarations below the glue block are byte-for-byte Parse.hs, Interact.hs,
-- PrologData.hs, Subst.hs, Engine.hs and the pure part of Main.hs
-- (Version.hs is the string constant). The port is the glue header (the
-- same one Parser.hs carries: Prelude functions NanoPrelude does not have,
-- each the standard definition, plus lines and nub), the two input files as
-- string constants, and the entry point.
--
-- Upstream main: read runtime_files/stdlib, then
-- replicateM_ 200 (print (hash (loop startDb stdin))).
-- SIM SCALE (user ruling 2026-07-30): fastN = 200 repetitions of the FAST
-- stdin; simN = 1 -- one pass over the same input, same hash.
module Prolog where
import Prelude()
import NanoClasses hiding (and, elem, head, lines, max, or, reverse, tail, take, words)
import Primitives (primCharLE, primCharEQ)

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

take :: Int -> [a] -> [a]
take k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : take (k-1) ys }

drop :: Int -> [a] -> [a]
drop k xs = if k <= 0 then xs else case xs of { [] -> []; (_:ys) -> drop (k-1) ys }

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = case xs of
                   []     -> []
                   (y:ys) -> if p y then dropWhile p ys else xs

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

repeat :: a -> [a]
repeat x = x : repeat x

elem :: Eq a => a -> [a] -> Bool
elem x = any (\y -> y == x)

notElem :: Eq a => a -> [a] -> Bool
notElem x ys = not (elem x ys)

and :: [Bool] -> Bool
and = all id

or :: [Bool] -> Bool
or = any id

max :: Int -> Int -> Int
max a b = if a > b then a else b

words :: String -> [String]
words s = case dropWhile isSpace s of
            [] -> []
            s' -> fst wsw : words (snd wsw)
                  where wsw = breakSp s'

breakSp :: String -> (String, String)
breakSp []     = ([], [])
breakSp (c:cs) = if isSpace c then ([], c:cs)
                 else case breakSp cs of (a, b) -> (c:a, b)

-- a non-terminating bottom: the parse of a valid input never reaches one
error :: String -> a
error s = error s

-- the character predicates compare Chars with the machine primitive, not
-- via primOrd and an Int comparison: this is a lexer, they are the hottest
-- code in the program.
isDigit :: Char -> Bool
isDigit c = primCharLE (primChr 48) c && primCharLE c (primChr 57)

isUpper :: Char -> Bool
isUpper c = primCharLE (primChr 65) c && primCharLE c (primChr 90)

isLower :: Char -> Bool
isLower c = primCharLE (primChr 97) c && primCharLE c (primChr 122)

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAlpha c || isDigit c

isSpace :: Char -> Bool
isSpace c = primCharEQ c (primChr 32) || primCharEQ c (primChr 9)
         || primCharEQ c (primChr 10) || primCharEQ c (primChr 13)
         || primCharEQ c (primChr 12) || primCharEQ c (primChr 11)

ord :: Char -> Int
ord = primOrd

fromEnum :: Char -> Int
fromEnum = primOrd

toEnum :: Int -> Char
toEnum = primChr

chr :: Int -> Char
chr = primChr

lines :: String -> [String]
lines s = case breakNl s of
            (l, [])      -> [l]
            (l, (_:rest)) -> l : lines rest
  where breakNl []     = ([], [])
        breakNl (c:cs) = if primCharEQ c (primChr 10) then ([], c:cs)
                         else case breakNl cs of (a, b) -> (c:a, b)

nub :: Eq a => [a] -> [a]
nub = go []
  where go seen []     = []
        go seen (x:xs) = if x `elem` seen then go seen xs
                         else x : go (x : seen) xs

version :: String
version = "stack based"

-- NanoClasses' (<) is Int-only; the database keeps its clause groups in
-- lexicographic Atom order. ltStr is the standard String ordering,
-- substituted at the two comparison sites (clausesFor, addClause).
ltStr :: String -> String -> Bool
ltStr []     []     = False
ltStr []     _      = True
ltStr _      []     = False
ltStr (x:xs) (y:ys) = if primCharEQ x y then ltStr xs ys
                      else primCharLE x y && not (primCharEQ x y)


infixr 6 `seQ`
infixl 5 `doo`
infixr 4 `orelse`

--- Type definition:

type Parser a = [Char] -> [(a,[Char])]

-- A parser is a function which maps an input stream of characters into
-- a list of pairs each containing a parsed value and the remainder of the
-- unused input stream.  This approach allows us to use the list of
-- successes technique to detect errors (i.e. empty list ==> syntax error).
-- it also permits the use of ambiguous grammars in which there may be more
-- than one valid parse of an input string.

--- Primitive parsers:

-- faiL     is a parser which always fails.
-- okay v   is a parser which always succeeds without consuming any characters
--          from the input string, with parsed value v.
-- tok w    is a parser which succeeds if the input stream begins with the
--          string (token) w, returning the matching string and the following
--          input.  If the input does not begin with w then the parser fails.
-- sat p    is a parser which succeeds with value c if c is the first input
--          character and c satisfies the predicate p.

faiL        :: Parser a
faiL inn      = []

okay        :: a -> Parser a
okay v inn    = [(v,inn)]

tok         :: [Char] -> Parser [Char]
tok w inn     = [(w, drop n inn) | w == take n inn]
               where n = length w

sat         :: (Char -> Bool) -> Parser Char
sat p []     = []
sat p (c:inn) = [ (c,inn) | p c ]

--- Parser combinators:

-- p1 `orelse` p2 is a parser which returns all possible parses of the input
--                string, first using the parser p1, then using parser p2.
-- p1 `seQ` p2    is a parser which returns pairs of values (v1,v2) where
--                v1 is the result of parsing the input string using p1 and
--                v2 is the result of parsing the remaining input using p2.
-- p `doo` f       is a parser which behaves like the parser p, but returns
--                the value f v wherever p would have returned the value v.
--
-- just p         is a parser which behaves like the parser p, but rejects any
--                parses in which the remaining input string is not blank.
-- sp p           behaves like the parser p, but ignores leading spaces.
-- sptok w        behaves like the parser tok w, but ignores leading spaces.
--
-- many p         returns a list of values, each parsed using the parser p.
-- many1 p        parses a non-empty list of values, each parsed using p.
-- listOf p s     parses a list of input values using the parser p, with
--                separators parsed using the parser s.

orelse             :: Parser a -> Parser a -> Parser a
orelse p1 p2 inn = p1 inn ++ p2 inn

seQ                :: Parser a -> Parser b -> Parser (a,b)
seQ p1 p2 inn    = [((v1,v2),inn2) | (v1,inn1) <- p1 inn, (v2,inn2) <- p2 inn1]

doo                 :: Parser a -> (a -> b) -> Parser b
doo p f inn       = [(f v, inn1) | (v,inn1) <- p inn]

just               :: Parser a -> Parser a
just p inn           = [ (v,"") | (v,inn')<- p inn, dropWhile (' '==) inn' == "" ]

sp                 :: Parser a -> Parser a
sp p                = p . dropWhile (' '==)

sptok              :: [Char] -> Parser [Char]
sptok               =  sp . tok

many               :: Parser a  -> Parser [a]
many p              = q
                      where q = ((p `seQ` q) `doo` makeList) `orelse` (okay [])

many1              :: Parser a -> Parser [a]
many1 p             = p `seQ` many p `doo` makeList

listOf             :: Parser a -> Parser b -> Parser [a]
listOf p s          = p `seQ` many (s `seQ` p) `doo` nonempty
                      `orelse` okay []
                      where nonempty (x,xs) = x:(map snd xs)

--- Internals:

makeList       :: (a,[a]) -> [a]
makeList (x,xs) = x:xs

{-
-- an attempt to optimise the performance of the standard prelude function
-- `take' in Haskell B 0.99.3 gives the wrong semantics.  The original
-- definition, given below works correctly and is used in the above.

safetake              :: (Integral a) => a -> [b] -> [b]
safetake  _     []     =  []
safetake  0     _      =  []
safetake (n+1) (x:xs)  =  x : safetake n xs
-}
--- End of Parse.hs

-- The functions defined in this module provide basic facilities for
-- writing line-oriented interactive programs (i.e. a function mapping
-- an input string to an appropriate output string).  These definitions
-- are an enhancement of thos in B+W 7.8
--
-- skip p         is an interactive program which consumes no input, produces
--                no output and then behaves like the interactive program p.
-- end            is an interactive program which ignores the input and
--                produces no output.
-- writeln txt p  is an interactive program which outputs the message txt
--                and then behaves like the interactive program p
-- readch act def is an interactive program which reads the first character c
--                from the input stream and behaves like the interactive
--                program act c.  If the input character stream is empty,
--                readch act def prints the default string def and terminates.
--
-- readln p g     is an interactive program which prints the prompt p and
--                reads a line (upto the first carriage return, or end of
--                input) from the input stream.  It then behaves like g line.
--                Backspace characters included in the input stream are
--                interpretted in the usual way.

type Interactive = String -> String

--- Interactive program combining forms:

skip                 :: Interactive -> Interactive
skip p inn             = p inn    -- a dressed up identity function

end                  :: Interactive
end inn                = ""

writeln              :: String -> Interactive -> Interactive
writeln txt p inn      = txt ++ p inn

readch               :: (Char -> Interactive) -> String -> Interactive
readch act def ""     = def
readch act def (c:cs) = act c cs

readln               :: String -> (String -> Interactive) -> Interactive
readln prompt g inn    = prompt ++ lineOut 0 line ++ "\n"
                               ++ g (noBackSpaces line) input'
                        where line     = before '\n' inn
                              input'   = after  '\n' inn
                              after x  = tail . dropWhile (x/=)
                              before x = takeWhile (x/=)

--- Filter out backspaces etc:

rubout  :: Char -> Bool
rubout c = (c=='\DEL' || c=='\BS')

lineOut                      :: Int -> String -> String
lineOut n ""                  = ""
lineOut n (c:cs)
          | n>0  && rubout c  = "\BS \BS" ++ lineOut (n-1) cs
          | n==0 && rubout c  = lineOut 0 cs
          | otherwise         = c:lineOut (n+1) cs

noBackSpaces :: String -> String
noBackSpaces  = reverse . delete 0 . reverse
                where delete n ""          = ""
                      delete n (c:cs)
                               | rubout c  = delete (n+1) cs
                               | n>0       = delete (n-1) cs
                               | otherwise = c:delete 0 cs

--- End of Interact.hs


infix 6 :==

--- Prolog Terms:

type Id       = (Int,String)
type Atom     = String
data Term     = Var Id | Struct Atom [Term]
data Clause   = Term :== [Term]
data Database = Db [(Atom,[Clause])]

instance Eq Term where
    Var v       == Var w       =  v==w
    Struct a ts == Struct b ss =  a==b && ts==ss
    _           == _           =  False

--- Determine the list of variables in a term:

varsIn              :: Term -> [Id]
varsIn (Var i)       = [i]
varsIn (Struct i ts) = (nub . concat . map varsIn) ts

renameVars                  :: Int -> Term -> Term
renameVars lev (Var (n,s))   = Var (lev,s)
renameVars lev (Struct s ts) = Struct s (map (renameVars lev) ts)

--- Functions for manipulating databases (as an abstract datatype)

emptyDb      :: Database
emptyDb       = Db []

renClauses                  :: Database -> Int -> Term -> [Clause]
renClauses db n (Var _)      = []
renClauses db n (Struct a _) = [ r tm:==map r tp | (tm:==tp)<-clausesFor a db ]
                               where r = renameVars n

clausesFor           :: Atom -> Database -> [Clause]
clausesFor a (Db rss) = case dropWhile (\(n,rs) -> ltStr n a) rss of
                         []         -> []
                         ((n,rs):_) -> if a==n then rs else []

addClause (Db rss) r@(Struct a _ :== _)
           = Db (update rss)
             where update []            = [(a,[r])]
                   update (h@(n,rs):rss')
                          | n==a        = (n,rs++[r]) : rss'
                          | ltStr n a   = h : update rss'
                          | otherwise   = (a,[r]) : h : rss'

--- Output functions (defined as instances of Text):

instance Show Term where
  showsPrec p (Var (n,s))
              | n==0        = showString s
              | otherwise   = showString s . showChar '_' . shows n
  showsPrec p (Struct a []) = showString a
  showsPrec p (Struct a ts) = showString a . showChar '('
                                           . showWithSep "," ts
                                           . showChar ')'

instance Show Clause where
   showsPrec p (t:==[]) = shows t . showChar '.'
   showsPrec p (t:==gs) = shows t . showString ":=="
                                 . showWithSep "," gs
                                 . showChar '.'

instance Show Database where
    showsPrec p (Db [])  = showString "-- Empty Database --\n"
    showsPrec p (Db rss) = foldr1 (\u v-> u . showChar '\n' . v)
                                  [ showWithTerm "\n" rs | (i,rs)<-rss ]

--- Local functions for use in defining instances of Text:

showWithSep          :: Show a => String -> [a] -> ShowS
showWithSep s [x]     = shows x
showWithSep s (x:xs)  = shows x . showString s . showWithSep s xs

showWithTerm         :: Show a => String -> [a] -> ShowS
showWithTerm s xs     = foldr1 (.) [shows x . showString s | x<-xs]

--- String parsing functions for Terms and Clauses:
--- Local definitions:

letter       :: Parser Char
letter        = sat (\c -> isAlpha c || isDigit c || c `elem` ":;+=-*&%$#@?/.~!")

variable     :: Parser Term
variable      = sat isUpper `seQ` many letter `doo` makeVar
                where makeVar (initial,rest) = Var (0,(initial:rest))

struct       :: Parser Term
struct        = many letter `seQ` (sptok "(" `seQ` termlist `seQ` sptok ")"
                                       `doo` (\(o,(ts,c))->ts)
                                  `orelse`
                                   okay [])
                `doo` (\(name,terms)->Struct name terms)

--- Exports:

term         :: Parser Term
term          = sp (variable `orelse` struct)

termlist     :: Parser [Term]
termlist      = listOf term (sptok ",")

clause       :: Parser Clause
clause        = sp struct `seQ` (sptok ":==" `seQ` listOf term (sptok ",")
                                 `doo` (\(from,body)->body)
                                `orelse` okay [])
                          `seQ` sptok "."
                     `doo` (\(head,(goals,dot))->head:==goals)

--- End of PrologData.hs


infixr 3 @@
infix  4 ->>

--- Substitutions:

type Subst = Id -> Term

-- substitutions are represented by functions mapping identifiers to terms.
--
-- apply s   extends the substitution s to a function mapping terms to terms
-- nullSubst is the empty substitution which maps every identifier to the
--           same identifier (as a term).
-- i ->> t   is the substitution which maps the identifier i to the term t,
--           but otherwise behaves like nullSubst.
-- s1 @@ s2  is the composition of substitutions s1 and s2
--           N.B.  apply is a monoid homomorphism from (Subst,nullSubst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  apply (s1 @@ s2) = apply s1 . apply s2
--                    s @@ nullSubst = s = nullSubst @@ s

apply                   :: Subst -> Term -> Term
apply s (Var i)          = s i
apply s (Struct a ts)    = Struct a (map (apply s) ts)

nullSubst               :: Subst
nullSubst i              = Var i

(->>)                   :: Id -> Term -> Subst
(->>) i t j | j==i       = t
            | otherwise  = Var j

(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = apply s1 . s2

--- Unification:

-- unify t1 t2 returns a list containing a single substitution s which is
--             the most general unifier of terms t1 t2.  If no unifier
--             exists, the list returned is empty.

unify :: Term -> Term -> [Subst]
unify (Var x)       (Var y)       = if x==y then [nullSubst] else [x->>Var y]
unify (Var x)       t2            = [ x ->> t2 | not (x `elem` varsIn t2) ]
unify t1            (Var y)       = [ y ->> t1 | not (y `elem` varsIn t1) ]
unify (Struct a ts) (Struct b ss) = [ u | a==b, u<-listUnify ts ss ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify []     []     = [nullSubst]
listUnify []     (r:rs) = []
listUnify (t:ts) []     = []
listUnify (t:ts) (r:rs) = [ u2 @@ u1 | u1<-unify t r,
                                       u2<-listUnify (map (apply u1) ts)
                                                     (map (apply u1) rs) ]

--- End of Subst.hs


--- Calculation of solutions:

-- the stack based engine maintains a stack of triples (s,goal,alts)
-- corresponding to backtrack points, where s is the substitution at that
-- point, goal is the outstanding goal and alts is a list of possible ways
-- of extending the current proof to find a solution.  Each member of alts
-- is a pair (tp,u) where tp is a new subgoal that must be proved and u is
-- a unifying substitution that must be combined with the substitution s.
--
-- the list of relevant clauses at each step in the execution is produced
-- by attempting to unify the head of the current goal with a suitably
-- renamed clause from the database.

type Stack = [ (Subst, [Term], [Alt]) ]
type Alt   = ([Term], Subst)

alts       :: Database -> Int -> Term -> [Alt]
alts db n g = [ (tp,u) | (tm:==tp) <- renClauses db n g, u <- unify g tm ]

-- The use of a stack enables backtracking to be described explicitly,
-- in the following `state-based' definition of prove:

prove      :: Database -> [Term] -> [Subst]
prove db gl = solve 1 nullSubst gl []
 where
   solve :: Int -> Subst -> [Term] -> Stack -> [Subst]
   solve n s []     ow          = s : backtrack n ow
   solve n s (g:gs) ow
                    | g==theCut = solve n s gs (cut ow)
                    | otherwise = choose n s gs (alts db n (apply s g)) ow

   choose :: Int -> Subst -> [Term] -> [Alt] -> Stack -> [Subst]
   choose n s gs []          ow = backtrack n ow
   choose n s gs ((tp,u):rs) ow = solve (n+1) (u@@s) (tp++gs) ((s,gs,rs):ow)

   backtrack                   :: Int -> Stack -> [Subst]
   backtrack n []               = []
   backtrack n ((s,gs,rs):ow)   = choose (n-1) s gs rs ow


--- Special definitions for the cut predicate:

theCut    :: Term
theCut     = Struct "!" []

cut                  :: Stack -> Stack
cut (top:(s,gl,_):ss) = top:(s,gl,[]):ss
cut ss                = ss

--- End of Engine.hs

--- Command structure and parsing (upstream Main.hs, the pure part):

data Command = Fact Clause | Query [Term] | Show | Error | Quit | NoChange

command :: Parser Command
command  = just (sptok "bye" `orelse` sptok "quit") `doo` (\quit->Quit)
               `orelse`
           just (okay NoChange)
               `orelse`
           just (sptok "??") `doo` (\show->Show)
               `orelse`
           just clause `doo` Fact
               `orelse`
           just (sptok "?-" `seQ` termlist) `doo` (\(q,ts)->Query ts)
               `orelse`
           okay Error

loop             :: Database -> String -> String
loop db           = readln "> " (exec db . fst . head . command)

exec             :: Database -> Command -> String -> String
exec db (Fact r)  = skip                              (loop (addClause db r))
exec db (Query q) = demonstrate db q
exec db Show      = writeln (show db)                 (loop db)
exec db Error     = writeln "I don't understand\n"    (loop db)
exec db Quit      = writeln "Thank you and goodbye\n" end
exec db NoChange  = skip                              (loop db)

solution      :: [Id] -> Subst -> [String]
solution vs s  = [ show (Var i) ++ " = " ++ show v
                                | (i,v) <- [ (i,s i) | i<-vs ], v /= Var i ]

demonstrate     :: Database -> [Term] -> Interactive
demonstrate db q = printOut (map (solution vs) (prove db q))
 where vs               = (nub . concat . map varsIn) q
       printOut []      = writeln "no.\n"     (loop db)
       printOut ([]:bs) = writeln "yes.\n"    (loop db)
       printOut (b:bs)  = writeln (doLines b) (nextReqd bs)
       doLines          = foldr1 (\xs ys -> xs ++ "\n" ++ ys)
       nextReqd bs      = writeln " "
                            (readch (\c->if c==';'
                                           then writeln ";\n" (printOut bs)
                                           else writeln "\n"  (loop db)) "")


-- the FAST stdin (prolog.faststdin) and runtime_files/stdlib: the
-- bare-metal target has no file system, so both become constants.
stdlibInput :: String
stdlibInput = "This file contains a list of predicate definitions that will automatically\nbe read into Mini Prolog at the beginning of a session.  Each clause in this\nfile must be entered on a single line and lines containing syntax errors are\nalways ignored.  This includes the first few lines of this file and provides\na simple way to include comments.\n\nappend(nil,X,X).\nappend(cons(X,Y),Z,cons(X,W)):==append(Y,Z,W).\n\nequals(X,X).\n\nnot(X):==X,!,false.\nnot(X).\n\nor(X,Y):==X.\nor(X,Y):==Y.\n\ntrue.\n\nEnd of stdlib\n"

queryInput :: String
queryInput = "??\n?- append(cons(1,nil),cons(2,nil),X)\n;\n?- append(X,Y,cons(1,cons(2,nil)))\n;;;\n?- append(cons(1,nil),cons(2,nil),cons(1,cons(2,nil)))\n?- append(cons(1,nil),cons(2,nil),cons(1,cons(3,nil)))\nparent(Child,Parent):==father(Child,Parent).\nparent(Child,Parent):==mother(Child,Parent).\ngrandparent(GChild,Gparent):==parent(GChild,Parent),parent(Parent,Gparent).\nfather(charles,princePhilip).\nmother(charles,theQueen).\nfather(anne,princePhilip).\nmother(anne,theQueen).\nfather(andrew,princePhilip).\nmother(andrew,theQueen).\nfather(edward,princePhilip).\nmother(edward,theQueen).\nmother(theQueen,theQueenMother).\nfather(william,charles).\nmother(william,diana).\nfather(harry,charles).\nmother(harry,diana).\n?- grandparent(X,theQueenMother)\n;;;;\n?- grandparent(harry,Who)\n;;\nsibling(One,Tother) :== parent(One,X),parent(Tother,X).\n?- sibling(harry,Who)\n;;;;\nnewsib(One,Tother) :== parent(One,X),!,parent(Tother,X).\n?- newsib(harry,Who)\n;;\nnewsib1(O,T):==parent(O,X),!,parent(T,X),not(equals(O,T)).\n?- newsib1(harry,Who)\n;\nbye\n"

startDb :: Database
startDb = foldl addClause emptyDb
            [ r | ((r,""):_) <- map clause (lines stdlibInput) ]

hash :: String -> Int
hash = foldl (\acc c -> ord c + acc*31) 0

fastN, simN :: Int
fastN = 200
simN = 1

bench :: Int
bench = hash (loop startDb queryInput)

main :: Int
main = bench
