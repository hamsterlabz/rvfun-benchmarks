-- Knights.hs - nofib spectral/knights, NanoPrelude dialect.
-- The four upstream modules (ChessSetList, KnightHeuristic, Queue, Main)
-- merged into one file, which is this dialect's one-file-per-benchmark
-- convention; the algorithm is untouched. Glue only:
--   * Queue a = [a], as upstream.
--   * upstream's `instance Ord ChessSet` makes ANY board <= any other, so
--     quickSort over [(Int,ChessSet)] orders on the Int alone; qsortP does
--     exactly that, with upstream's partition (< pivot, then >= pivot).
--   * last/init/(!!) are spelled out, and strings are [Int] of character
--     codes (this dialect has no Char), so the board printer is carried
--     over as-is on that representation.
-- Upstream main prints `length (printTour ss)`; for `8 1` that is 232,
-- which this port reproduces exactly (checked against knights.faststdout).
module Knights where
import Prelude()
import NanoPrelude

type Tile = (Int, Int)

data ChessSet = Board Int Int Tile [Tile]

data Direction = UL | UR | DL | DR | LU | LD | RU | RD

-- list helpers not in NanoPrelude ------------------------------------------

appendL :: [a] -> [a] -> [a]
appendL []     ys = ys
appendL (x:xs) ys = x : appendL xs ys

lastL :: [a] -> a
lastL (x:[])   = x
lastL (_:xs)   = lastL xs

idx :: [a] -> Int -> a
idx (x:_)  0 = x
idx (_:xs) n = idx xs (n-1)

enumTo :: Int -> Int -> [Int]
enumTo lo hi = if lo > hi then [] else lo : enumTo (lo+1) hi

takeL :: Int -> [a] -> [a]
takeL k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : takeL (k-1) ys }

-- upstream quickSort, ordering on the Int of the pair only ------------------

qsortP :: [(Int, ChessSet)] -> [(Int, ChessSet)]
qsortP []     = []
qsortP (x:xs) =
  appendL (qsortP (filter (\y -> fst y < fst x) xs))
          (x : qsortP (filter (\y -> fst y >= fst x) xs))

-- ChessSetList -------------------------------------------------------------

createBoard :: Int -> Tile -> ChessSet
createBoard x t = Board x 1 t (t:[])

sizeBoard :: ChessSet -> Int
sizeBoard (Board s _ _ _) = s

noPieces :: ChessSet -> Int
noPieces (Board _ n _ _) = n

addPiece :: Tile -> ChessSet -> ChessSet
addPiece t (Board s n f ts) = Board s (n+1) f (t:ts)

deleteFirst :: ChessSet -> ChessSet
deleteFirst (Board s n f ts) = Board s (n-1) (lastL ts') ts'
  where ts' = init ts

positionPiece :: Int -> ChessSet -> Tile
positionPiece x (Board _ n _ ts) = idx ts (n - x)

lastPiece :: ChessSet -> Tile
lastPiece (Board _ _ _ (t:_)) = t

firstPiece :: ChessSet -> Tile
firstPiece (Board _ _ f _) = f

tileEq :: Tile -> Tile -> Bool
tileEq (a,b) (c,d) = a == c && b == d

isSquareFree :: Tile -> ChessSet -> Bool
isSquareFree t (Board _ _ _ ts) = all (\u -> not (tileEq t u)) ts

-- KnightHeuristic ----------------------------------------------------------

move :: Direction -> Tile -> Tile
move UL (x,y) = (x-1, y-2)
move UR (x,y) = (x+1, y-2)
move DL (x,y) = (x-1, y+2)
move DR (x,y) = (x+1, y+2)
move LU (x,y) = (x-2, y-1)
move LD (x,y) = (x-2, y+1)
move RU (x,y) = (x+2, y-1)
move RD (x,y) = (x+2, y+1)

startTour :: Tile -> Int -> ChessSet
startTour st size = createBoard size st

moveKnight :: ChessSet -> Direction -> ChessSet
moveKnight board dir = addPiece (move dir (lastPiece board)) board

canMoveTo :: Tile -> ChessSet -> Bool
canMoveTo (x,y) board =
  x >= 1 && x <= sze && y >= 1 && y <= sze && isSquareFree (x,y) board
  where sze = sizeBoard board

canMove :: ChessSet -> Direction -> Bool
canMove board dir = canMoveTo (move dir (lastPiece board)) board

possibleMoves :: ChessSet -> [Direction]
possibleMoves board =
  filter (canMove board) (UL:UR:DL:DR:LU:LD:RU:RD:[])

allDescend :: ChessSet -> [ChessSet]
allDescend board = map (moveKnight board) (possibleMoves board)

descAndNo :: ChessSet -> [(Int, ChessSet)]
descAndNo board =
  map (\x -> (length (possibleMoves (deleteFirst x)), x)) (allDescend board)

singleDescend :: ChessSet -> [ChessSet]
singleDescend board = map snd (filter (\yx -> fst yx == 1) (descAndNo board))

deadEnd :: ChessSet -> Bool
deadEnd board = length (possibleMoves board) == 0

canJumpFirst :: ChessSet -> Bool
canJumpFirst board = canMoveTo (firstPiece board) (deleteFirst board)

descendents :: ChessSet -> [ChessSet]
descendents board =
  if canJumpFirst board && deadEnd (addPiece (firstPiece board) board)
  then []
  else case length singles of
         0 -> map snd (qsortP (descAndNo board))
         1 -> singles
         _ -> []
  where singles = singleDescend board

tourFinished :: ChessSet -> Bool
tourFinished board = noPieces board == sze*sze && canJumpFirst board
  where sze = sizeBoard board

-- Main: depth-first search over the queue ----------------------------------

grow :: (Int, ChessSet) -> [(Int, ChessSet)]
grow (x,y) = map (\b -> (x+1, b)) (descendents y)

isFinished :: (Int, ChessSet) -> Bool
isFinished (_,y) = tourFinished y

root :: Int -> [(Int, ChessSet)]
root sze =
  map (\t -> (1 - sze*sze, startTour t sze))
      (concat (map (\x -> map (\y -> (x,y)) (enumTo 1 sze)) (enumTo 1 sze)))

depthSearch :: [(Int, ChessSet)] -> [(Int, ChessSet)]
depthSearch []    = []
depthSearch (q:qs) =
  if isFinished q then q : depthSearch qs
  else depthSearch (appendL (grow q) qs)

-- the board printer, verbatim from ChessSetList (strings are [Int] of
-- character codes in this dialect, so show/++ become showI/appendL)

showI :: Int -> [Int]
showI n = if n < 10 then (48+n):[] else appendL (showI (div n 10)) ((48 + mod n 10):[])

logTen :: Int -> Int
logTen 0 = 0
logTen x = 1 + logTen (div x 10)

spaces :: Int -> Int -> [Int]
spaces s y = replicate (logTen s - logTen y + 1) 32

assignMoveNo :: [Tile] -> Int -> Int -> [(Int,Int)]
assignMoveNo []        _    _ = []
assignMoveNo ((x,y):t) size z = (((y-1)*size)+x, z) : assignMoveNo t size (z-1)

printBoard :: Int -> [(Int,Int)] -> Int -> [Int]
printBoard s [] n =
  if n > s*s then []
  else if mod n s /= 0
       then appendL (42 : spaces (s*s) 1) (printBoard s [] (n+1))
       else appendL (42 : 10 : [])        (printBoard s [] (n+1))
printBoard s ((i,j):xs) n =
  if i == n && mod n s == 0
    then appendL (appendL (showI j) (10:[])) (printBoard s xs (n+1))
  else if i == n && mod n s /= 0
    then appendL (appendL (showI j) (spaces (s*s) j)) (printBoard s xs (n+1))
  else if mod n s /= 0
    then appendL (42 : spaces (s*s) 1) (printBoard s ((i,j):xs) (n+1))
    else appendL (42 : 10 : [])        (printBoard s ((i,j):xs) (n+1))

showBoard :: ChessSet -> [Int]
showBoard (Board sze n _ ts) = printBoard sze (qsortI (assignMoveNo ts sze n)) 1

qsortI :: [(Int,Int)] -> [(Int,Int)]
qsortI []     = []
qsortI (x:xs) =
  appendL (qsortI (filter (\y -> fst y < fst x) xs))
          (x : qsortI (filter (\y -> fst y >= fst x) xs))

-- printTour, verbatim: "\nKnights tour with <n> backtracking moves\n<board>"
s_hdr, s_tail :: [Int]
s_hdr  = 10:75:110:105:103:104:116:115:32:116:111:117:114:32:119:105:116:104:32:[]
s_tail = 32:98:97:99:107:116:114:97:99:107:105:110:103:32:109:111:118:101:115:10:[]

pp :: [(Int, ChessSet)] -> [Int]
pp []          = []
pp ((x,y):xs)  = appendL s_hdr (appendL (showI x) (appendL s_tail
                   (appendL (showBoard y) (pp xs))))

printTour :: Int -> Int -> [Int]
printTour size number = pp (takeL number (depthSearch (root size)))

hashS :: [Int] -> Int
hashS = foldl (\acc c -> c + acc*31) 0

-- SIM SCALE (user ruling 2026-07-30): fast* is the upstream FAST value,
-- sim* is what is actually run. Upstream FAST is `8 1`: an 8x8 board, the
-- first solution. That is what runs here too -- the heuristics find the
-- first tour with no backtracking, so the search is affordable.
fastSize, simSize, fastSols, simSols :: Int
fastSize = 8
simSize  = 8
fastSols = 1
simSols  = 1

-- upstream main prints `length (printTour ss)`; that length is the nofib
-- answer (232 for `8 1`). bench carries it plus the stream hash, so a
-- wrong tour cannot pass on length alone.
bench :: Int
bench = length tour + hashS tour
  where tour = printTour simSize simSols

main :: Int
main = bench
