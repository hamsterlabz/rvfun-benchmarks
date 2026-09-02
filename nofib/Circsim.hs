-- Circsim.hs - nofib spectral/circsim (O'Donnell's parallel circuit
-- simulator), NanoClasses dialect.  THE BENCHMARK BODY IS UPSTREAM'S,
-- UNTOUCHED: every declaration below the glue block is the unliterated
-- code of Main.lhs verbatim, minus its imports, minus the two putStr
-- display helpers (showPacket/showPackets, IO-only and unused by run),
-- and minus the IO main, with its tabs expanded to the standard 8-column
-- stops (token-identical for GHC; mhs layout reads a tab as one column).
-- The glue is this header (prelude names the
-- dialect lacks) and the entry point at the bottom.
--
-- Upstream main: forM_ [1..97] (print (run num_bits num_cycles)) with
-- FAST_OPTS = 8 4.  The 97 repetitions re-print byte-identical work
-- (timing chaff per the porting rules): fastR = 97, simR = 1.
-- SIM SCALE: the FAST input 8 4 is kept.  Consumption: the printed
-- [[Boolean]] rendered through show and folded with the nofib hash.
module Circsim where
import Prelude()
import NanoClasses
import Data.Records  -- glue: record syntax desugars through HasField

-- glue: prelude names the dialect lacks --------------------------------
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

splitAt :: Int -> [a] -> ([a], [a])
splitAt k xs = (take k xs, drop k xs)
  where drop n zs = if n <= 0 then zs else case zs of { [] -> []; (_:ys) -> drop (n-1) ys }

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

repeat :: a -> [a]
repeat x = x : repeat x

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((y:ys) : xss) = (y : map head' xss) : transpose (ys : map tail' xss)
  where head' (h:_) = h
        tail' (_:t) = t

or :: [Bool] -> Bool
or = any (\b -> b)

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

infixr 8 ^
(^) :: Int -> Int -> Int
b ^ e = if e <= 0 then 1 else b * (b ^ (e-1))

quotRem :: Int -> Int -> (Int, Int)
quotRem a b = (quot a b, rem a b)

bottom :: a
bottom = bottom

error :: String -> a
error _ = bottom

-- upstream Main.lhs, verbatim ------------------------------------------
data BinTree a b = Cell a
                   | Node b (BinTree a b) (BinTree a b)
                   deriving Show
put :: [a] -> BinTree a ()
put [x] = Cell x
put xs  = Node () (put fstHalf) (put sndHalf)
        where
          (fstHalf, sndHalf) = splitAt (length xs `div` 2) xs
get :: BinTree a b -> [a]
get (Cell x) = [x]
get (Node x l r) = get l ++ get r
upsweep :: (a -> a -> a)                -- node function
          -> BinTree a b
          -> (a, BinTree a (a,a))
upsweep f (Cell a)     = (a, Cell a)
upsweep f (Node x l r) = (f lv rv, Node (lv, rv) l' r')
    where
        (lv, l') = upsweep f l
        (rv, r') = upsweep f r
downsweep :: (a -> b -> c -> (c,c))     -- downsweep function
            -> c                        -- starting value
            -> BinTree d (a,b)
            -> BinTree c ()
downsweep g d (Cell x)     = Cell d
downsweep g d (Node (lv,rv) l r) = Node () l' r'
        where
        (dl, dr) = g lv rv d
        (l', r') = (downsweep g dl l, downsweep g dr r)
sweep_ud :: (a -> a -> a)                       -- upsweep node function
           -> (a -> a -> b -> (b,b))            -- downsweep function
           -> b                                 -- root input
           -> BinTree a c
           -> (a, BinTree b ())
sweep_ud up down u t
        = (ans, downsweep down u t')
        where (ans, t') = upsweep up t
scanL :: (a -> a -> a) -> a -> [a] -> (a,[a])
scanL f u xs = (up_ans, get t')
        where
        (up_ans, t') = sweep_ud f down u (put xs)
        down l r x = (x, f x l)
scanR :: (a -> a -> a) -> a -> [a] -> (a,[a])
scanR f u xs = (up_ans, get t')
        where
        (up_ans, t') = sweep_ud f down u (put xs)
        down l r x = (f r x, x)
scanlr :: (a -> a -> a)                 -- left to right function
         -> (a -> a -> a)                       -- right to left function
         -> a                                   -- left input
         -> a                                   -- right input
         -> [a]
         -> ((a,a), [(a,a)])
scanlr f g lu ru xs = (ans, get t)
    where
      ((l_ans,r_ans), t) = sweep_ud up down (lu,ru) (put xs')
      ans = (g r_ans ru, f lu l_ans)
      xs' = map (\x -> (x,x)) xs
      up (lx,ly) (rx,ry) = (f lx rx, g ly ry)
      down (lx,ly) (rx,ry) (a,b) = ((a, g ry b), (f a lx, b))
type Circuit a =        (Int,           -- circuit size
                        [Label],        -- location of inputs
                        [Label],        -- location of outputs
                        [State a])
type Label = (String, Pid)
type Pid = Int
data Component
        = None  -- no component
        | Inp   -- input to the entire circuit
        | Outp  -- output from the entire circuit
        | Dff   -- delay flip flop
        | Inv   -- inverter
        | And2  -- 2-input and gate
        | Or2   -- 2-input or gate
        | Xor   -- exclusive or gate
        deriving (Eq, Show)
data State a = PS
  {pid        :: Int,           -- site identifier
   compType   :: Component,     -- component represented in the site
   pathDepth  :: Int,           -- path depth at which outputs become valid
   inports    :: [InPort a],    -- tags and latches for the inputs
   outports   :: [OutPort a]    -- tags and latches for the outputs
  }
type InPort a =
  (Pid, -- identifies processor that will supply the input signal
   Int, -- the output port number of the signal
   a)           -- latch to hold the input signal value
type OutPort a =
        (Int,   -- output port number for the signal value
        a,      -- latch to hold the signal value
        Bool,   -- need to send it to the left?
        Int,    -- distance to send to the left
        Bool,   -- need to send it to the right?
        Int)    -- distance to send to the right
{-
instance Show a => Show (State a) where
        showsPrec p (PS {pid=x, compType=c, pathDepth=d,
                         inports=ins, outports=outs})
                = showString "\npid       = " . shows x .
                  showString "\ncompType  = " . shows c .
                  showString "\npathDepth = " . shows d .
                  showString "\ninports   = " . shows ins .
                  showString "\noutports  = " . shows outs . showChar '\n'
-}
instance Show a => Show (State a) where
        showsPrec p (PS {pid=x, compType=c, pathDepth=d,
                         inports=ins, outports=outs})
                = shows (x,c,d,ins,outs) . showChar '\n'
{-
instance (Show a, Show b, Show c, Show d, Show e, Show f)
                => Show (a, b, c, d, e, f) where
    showsPrec p (a,b,c,d,e,f) = showChar '(' . shows a . showChar ',' .
                                                 shows b . showChar ',' .
                                                 shows c . showChar ',' .
                                                 shows d . showChar ',' .
                                                 shows e . showChar ',' .
                                                 shows f . showChar ')'
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
                => Show (a, b, c, d, e, f, g, h) where
    showsPrec p (a,b,c,d,e,f,g,h) = showChar '(' . shows a . showChar ',' .
                                                 shows b . showChar ',' .
                                                 shows c . showChar ',' .
                                                 shows d . showChar ',' .
                                                 shows e . showChar ',' .
                                                 shows f . showChar ',' .
                                                 shows g . showChar ',' .
                                                 shows h . showChar ')'
-}
nearest_power_of_two :: Int -> Int
nearest_power_of_two x = until (>=x) (*2) 1
pad_circuit :: Signal a => Circuit a -> Circuit a
pad_circuit (size, ins, outs, states)
        = (p2, ins, outs, take p2 (states ++ repeat emptyState))
    where
        p2 = nearest_power_of_two size
emptyState :: Signal a => State a
emptyState = PS {       pid = -1,
                        compType = None,
                        pathDepth = -1,
                        inports = [],
                        outports = []}
class (Eq a, Show a) => Signal a where
        zeroS, one :: a
        inv :: a -> a
        and2, or2, xor :: a -> a -> a

        inv x = if (x==one) then zeroS  else one

        and2 x y = if (x==one) && (y==one) then one else zeroS

        or2 x y = if (x==one) || (y==one) then one else zeroS

        xor x y = if (x==y) then one else zeroS
data Boolean = F | T
        deriving (Eq, Show)
instance Signal Boolean where
        zeroS = F
        one   = T
type Packet a =
        (Pid,   -- id of this packet
        Int,    -- output port number for the signal value
        a,      -- latch to hold the signal value
        Bool,   -- need to send it to the left?
        Int,    -- distance to send to the left
        Bool,   -- need to send it to the right?
        Int,    -- distance to send to the right
        Int)    -- extent
emptyPacket :: Signal a => Packet a
emptyPacket = (-1, -1, zeroS, False, 0, False, 0, 1)
send_right :: Packet a -> Packet a -> Packet a
send_right (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =
        if qra && dra>eb
          then (ia,sa,ma,qla,dla,qra,dra-eb,ea+eb)
          else (ib,sb,mb,qlb,dlb,qrb,drb,ea+eb)
send_left :: Packet a -> Packet a -> Packet a
send_left (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =
        if qlb && dlb>ea
          then (ib,sb,mb,qlb,dlb-ea,qrb,drb,ea+eb)
          else (ia,sa,ma,qla,dla,qra,dra,ea+eb)
send :: Signal a => [Packet a]
          -> ((Packet a, Packet a), [(Packet a, Packet a)])
send xs = scanlr send_right send_left emptyPacket emptyPacket xs
circuit_simulate :: Signal a => [[a]] -> Circuit a -> [[a]]
circuit_simulate inputs_list circuit
        = map collect_outputs (simulate inputs_list circuit)
collect_outputs :: Circuit a -> [a]
collect_outputs (size, ins, outs, states) = map get_output outs
   where
        get_output (label, p)
                 = third (head [ head (inports s) | s<-states, p==pid s])
        third (_,_,v) = v
simulate :: Signal a => [[a]] -> Circuit a -> [Circuit a]
simulate inputs_list circuit@(size, ins, outs, states)
        = tail (scanl (do_cycle cpd) circuit' inputs_list)
    where
        circuit' = (size, ins, outs, map init_dffs states)
        cpd = critical_path_depth circuit
do_cycle :: Signal a => Int -> Circuit a -> [a] -> Circuit a
do_cycle cpd (size, ins, outs, states) inputs = (size, ins, outs, states4)
    where
        states1 = map (store_inputs (zip ins inputs)) states
        states2 = do_sends 0 states1
        states3 = foldl sim_then_send states2 [1..cpd]
        sim_then_send state d = do_sends d (simulate_components d state)
        states4 = restore_requests states states3
restore_requests :: Signal a => [State a] -> [State a] -> [State a]
restore_requests old_states new_states
                = zipWith restore old_states new_states
     where
        restore os ns = ns { outports = zipWith restore_outport (outports os)
                                                    (outports ns) }
        restore_outport (p,_,ql,dl,qr,dq) (_,m,_,_,_,_) = (p,m,ql,dl,qr,dq)
do_sends :: Signal a => Int -> [State a] -> [State a]
do_sends d states = until (acknowledge d) (do_send d) states
acknowledge :: Signal a => Int -> [State a] -> Bool
acknowledge d states = not (or (map (check_requests . outports) states1))
    where
        check_requests xs = or (map check_lr_requests xs)
        check_lr_requests (p,m,ql,dl,qr,dr) = ql || qr
        states1 = map (check_depth d) states
do_send :: Signal a => Int -> [State a] -> [State a]
do_send d states = zipWith (update_io d) pss' states
    where
        states1 = map (check_depth d) states
        pss = (transpose . pad_packets) (map make_packet states1)
        send_results = map (snd . send) pss
        pss' = transpose send_results
update_io :: Signal a => Int -> [(Packet a,Packet a)] -> State a -> State a
update_io d lrps state = update_os (update_is state)
    where
        update_is state = state { inports = foldr update_i
                                                       (inports state) lrps }
        update_os state = if pathDepth state == d
                            then state { outports = zipWith update_o
                                                        lrps (outports state) }
                            else state
update_o :: Signal a => (Packet a, Packet a) -> OutPort a -> OutPort a
update_o (lp, rp) out = check_left lp (check_right rp out)
check_left (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)
        = if pqr && pdr>0
                then (p, m, ql, dl, qr, dr)             -- message blocked
                else (p, m, ql, dl, False, dr)          -- send right succeeded
check_right (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)
        = if pql && pdl>0
             then (p, m, ql, dl, qr, dr)                -- message blocked
             else (p, m, False, dl, qr, dr)             -- send left succeeded
update_i :: Signal a => (Packet a, Packet a) -> [InPort a] -> [InPort a]
update_i (l,r) ins = up_i l (up_i r ins)
up_i :: Signal a => Packet a -> [InPort a] -> [InPort a]
up_i (i, p, m', _, _, _, _, _) ins
        = map (compare_and_update (i,p,m')) ins
compare_and_update :: Signal a => InPort a -> InPort a -> InPort a
compare_and_update (i, p, m') (pid, port, m)
                = if (i, p) == (pid, port)
                    then (pid, port, m')
                    else (pid, port, m)
make_packet :: Signal a => State a -> [Packet a]
make_packet state = [ (pid state, p, m, ql, dl, qr, dr, 1)
                        | (p, m, ql, dl, qr, dr) <- outports state ]
pad_packets :: Signal a => [[Packet a]] -> [[Packet a]]
pad_packets pss = map pad pss
    where
        pad xs = take max_ps (xs ++ repeat emptyPacket)
        max_ps = maximum (map length pss)
check_depth :: Signal a => Int -> State a -> State a
check_depth d state
        = if pathDepth state == d
            then state
            else update_requests False state
update_requests :: Signal a => Bool -> State a -> State a
update_requests b state
        = state { outports = [ (p, m, b, dl, b, dr)
                               | (p, m, ql, dl, qr, dr) <- outports state ] }
simulate_components :: Signal a => Int -> [State a] -> [State a]
simulate_components depth states
        = map (simulate_component depth) states
simulate_component :: Signal a => Int -> State a -> State a
simulate_component d state = if d == pathDepth state && new_value/=Nothing
                                 then let Just v = new_value
                                      in update_outports state v
                                 else state
    where
        out_signals = [ sig | (_,_,sig) <- inports state]
        new_value = apply_component (compType state) out_signals
apply_component :: Signal a => Component -> [a] -> Maybe a
apply_component Inp _      = Nothing
apply_component Outp [x]   = Just x
apply_component Dff [x]    = Just x
apply_component Inv [x]    = Just (inv x)
apply_component And2 [x,y] = Just (and2 x y)
apply_component Or2 [x,y]  = Just (or2 x y)
apply_component Xor [x,y]  = Just (xor x y)
apply_component _ _        = error "Error: apply_component\n"
store_inputs :: Signal a => [(Label,a)] -> State a -> State a
store_inputs label_inputs state@(PS {compType=Inp})
        = head [ update_outports state value
                        | ((label, input_pid), value) <- label_inputs,
                                                pid state == input_pid ]
store_inputs label_inputs state = state
init_dffs :: Signal a => State a -> State a
init_dffs state = if compType state == Dff
                      then update_outports state zeroS
                      else state
critical_path_depth :: Signal a => Circuit a -> Int
critical_path_depth (size, ins, outs, states)
        = maximum (map pathDepth states)
input_values :: Signal a => Int -> [[a]]
input_values nbits = map binary [0..2^nbits-1]
    where
        binary n = map int2sig (reverse (take nbits (bin n ++ repeat 0)))
        int2sig s = if (s==0) then zeroS else one
        bin 0 = []
        bin n = r:bin q
              where
                (q,r) = n `quotRem` 2
update_outports :: Signal a => State a -> a -> State a
update_outports state value
                = state { outports = [ (p, value, ql, dl, qr, dr)
                                        | (p, m, ql, dl, qr, dr) <- outs ] }
    where
        outs = outports state
regs :: Signal a => Int -> Circuit a
regs bits = (size, is, os, states)
    where
        size = 1+7*bits
        is = ("sto",0): zipWith ilabel [0..] [ 7*x+1 | x <- [0..bits-1]]
        ilabel n pid = ("x" ++ show n, pid)
        os = zipWith olabel [0..] [ 7*x+7 | x <- [0..bits-1]]
        olabel n pid = ("y" ++ show n, pid)
        states = sto:concat (map (reg 0) [ 7*x+1 | x <- [0..bits-1]])
        sto = PS {      pid = 0,
                        compType  = Inp,
                        pathDepth = 0,
                        inports    = [],
                        outports   = [(0, zeroS, False,
                                       0, True, 8*(bits-1)+5)]
                }
reg :: Signal a => Pid -> Pid -> [State a]
reg sto n
  = [ PS { pid       = n,       -- x input -------------------------------------
             compType  = Inp,
             pathDepth = 0,
             inports    = [],
             outports   = [(0, zeroS, False, 0, True, 4)]       -- lower and
           },
        PS { pid       = n+1,   -- dff -----------------------------------------
             compType  = Dff,
             pathDepth = 1,
             inports    = [(n+5, 0, zeroS)],
             outports   = [(0, zeroS, False, 0, True, 5)]       -- y output
           },
        PS { pid       = n+2,   -- inv -----------------------------------------
             compType  = Inv,
             pathDepth = 1,
             inports    = [(sto, 0, zeroS)],                    -- sto
             outports   = [(0, zeroS, False, 0, True, 1)]       -- upper and
           },
        PS { pid       = n+3, -- upper and -------------------------------------
             compType  = And2,
             pathDepth = 2,
             inports    = [(n+1, 0, zeroS),             -- dff
                          (n+2, 0, zeroS)],             -- inv
             outports   = [(0, zeroS, False, 0, True, 2)]       -- or
           },
        PS { pid       = n+4,   -- lower and -----------------------------------
             compType  = And2,
             pathDepth = 1,
             inports    = [(sto, 0, zeroS),             -- sto input
                          (n, 0, zeroS)],               -- x input
             outports   = [(0, zeroS, False, 0, True, 1)]       -- or
           },
        PS { pid       = n+5,   -- or ------------------------------------------
             compType  = Or2,
             pathDepth = 3,
             inports    = [(n+3, 0, zeroS),             -- upper and
                          (n+4, 0, zeroS)],             -- lower and
             outports   = [(0, zeroS, True, 4, False, 0)]       -- dff
           },
        PS { pid        = n+6,  -- output y ------------------------------------
             compType   = Outp,
             pathDepth  = 4,
             inports    = [(n+1, 0, zeroS)],            -- dff
             outports   = []
           }
        ]
run :: Int -> Int -> [[Boolean]]
run num_bits num_cycles = circuit_simulate cycles example
        where
        example = pad_circuit (regs num_bits)
        cycles = take num_cycles (repeat inputs)
        inputs = take (num_bits + 1) (repeat T)

-- entry point (upstream: forM_ [1..97] (print (run num_bits num_cycles)))
fastR, simR :: Int
fastR = 97
simR = 1

num_bits, num_cycles :: Int
num_bits = 8
num_cycles = 4

hashS :: String -> Int
hashS = foldl (\acc c -> primOrd c + acc*31) 0

main :: Int
main = hashS (show (run num_bits num_cycles))
