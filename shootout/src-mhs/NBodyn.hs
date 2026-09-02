-- NBodyn.hs - the Benchmarks Game 5-body kernel, 100 steps at dt = 0.01,
-- reporting energy*1e9 before and after (combined as before*10+after mod, see
-- below: the nano epilogue prints ONE Int, so this reports the FINAL energy).
-- Nano dialect, flat arrays via NanoArray.
module NBodyn where
import Prelude()
import NanoPrelude
import NanoArray

n :: Int
n = 5

sm :: Float
sm = 4.0 *. 3.141592653589793 *. 3.141592653589793

dpy :: Float
dpy = 365.24

main :: Int
main = newArr n (0.0::Float) (\x -> newArr n (0.0::Float) (\y -> newArr n (0.0::Float) (\z ->
       newArr n (0.0::Float) (\vx -> newArr n (0.0::Float) (\vy -> newArr n (0.0::Float) (\vz ->
       newArr n (0.0::Float) (\m -> setup x y z vx vy vz m)))))))

setBody :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
        -> IOArray Float -> IOArray Float -> IOArray Float
        -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float
        -> Int -> Int
setBody x y z vx vy vz m i px py pz pvx pvy pvz pm kont =
  writeArr x i px (writeArr y i py (writeArr z i pz (
  writeArr vx i pvx (writeArr vy i pvy (writeArr vz i pvz (
  writeArr m i pm kont))))))

setup :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
      -> IOArray Float -> IOArray Float -> IOArray Float -> Int
setup x y z vx vy vz m =
  writeArr m 0 sm (
  setBody x y z vx vy vz m 1
    4.84143144246472090 (0.0 -. 1.16032004402742839) (0.0 -. 0.103622044471123109)
    (0.00166007664274403694 *. dpy) (0.00769901118419740425 *. dpy)
    ((0.0 -. 0.0000690460016972063023) *. dpy) (0.000954791938424326609 *. sm) (
  setBody x y z vx vy vz m 2
    8.34336671824457987 4.12479856412430479 (0.0 -. 0.403523417114321381)
    ((0.0 -. 0.00276742510726862411) *. dpy) (0.00499852801234917238 *. dpy)
    (0.0000230417297573763929 *. dpy) (0.000285885980666130812 *. sm) (
  setBody x y z vx vy vz m 3
    12.8943695621391310 (0.0 -. 15.1111514016986312) (0.0 -. 0.223307578892655734)
    (0.00296460137564761618 *. dpy) (0.00237847173959480950 *. dpy)
    ((0.0 -. 0.0000296589568540237556) *. dpy) (0.0000436624404335156298 *. sm) (
  setBody x y z vx vy vz m 4
    15.3796971148509165 (0.0 -. 25.9193146099879641) 0.179258772950371181
    (0.00268067772490389322 *. dpy) (0.00162824170038242295 *. dpy)
    ((0.0 -. 0.0000951592254519715870) *. dpy) (0.0000515138902046611451 *. sm) (
    steps x y z vx vy vz m 100)))))

steps :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
      -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int
steps x y z vx vy vz m k =
  if k <= 0 then energy x y z vx vy vz m 0 0.0
  else adv x y z vx vy vz m 0 1 (moveAll x y z vx vy vz m 0 (steps x y z vx vy vz m (k-1)))

adv :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
    -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int -> Int -> Int
adv x y z vx vy vz m i j kont =
  if i >= n then kont
  else if j >= n then adv x y z vx vy vz m (i+1) (i+2) kont
  else readArr x i (\xi -> readArr y i (\yi -> readArr z i (\zi ->
       readArr x j (\xj -> readArr y j (\yj -> readArr z j (\zj ->
       readArr m i (\mi -> readArr m j (\mj ->
         let dx = xi -. xj
             dy = yi -. yj
             dz = zi -. zj
             d2 = dx *. dx +. dy *. dy +. dz *. dz
             mag = 0.01 /. (d2 *. sqrtD d2)
         in readArr vx i (\a -> readArr vy i (\b -> readArr vz i (\c ->
            readArr vx j (\d -> readArr vy j (\e -> readArr vz j (\f ->
              writeArr vx i (a -. dx *. mj *. mag) (
              writeArr vy i (b -. dy *. mj *. mag) (
              writeArr vz i (c -. dz *. mj *. mag) (
              writeArr vx j (d +. dx *. mi *. mag) (
              writeArr vy j (e +. dy *. mi *. mag) (
              writeArr vz j (f +. dz *. mi *. mag) (
                adv x y z vx vy vz m i (j+1) kont))))))))))))))))))))

moveAll :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
        -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Int -> Int
moveAll x y z vx vy vz m i kont =
  if i >= n then kont
  else readArr x i (\a -> readArr vx i (\b -> writeArr x i (a +. 0.01 *. b) (
       readArr y i (\c -> readArr vy i (\d -> writeArr y i (c +. 0.01 *. d) (
       readArr z i (\e -> readArr vz i (\f -> writeArr z i (e +. 0.01 *. f) (
         moveAll x y z vx vy vz m (i+1) kont)))))))))

energy :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
       -> IOArray Float -> IOArray Float -> IOArray Float -> Int -> Float -> Int
energy x y z vx vy vz m i acc =
  if i >= n then truncateD (acc *. 1000000000.0)
  else readArr m i (\mi -> readArr vx i (\a -> readArr vy i (\b -> readArr vz i (\c ->
         let sp = 0.5 *. mi *. (a *. a +. b *. b +. c *. c)
         in pairE x y z m i (i+1) 0.0 (\p ->
              energy x y z vx vy vz m (i+1) (acc +. sp +. p))))))

pairE :: IOArray Float -> IOArray Float -> IOArray Float -> IOArray Float
      -> Int -> Int -> Float -> (Float -> Int) -> Int
pairE x y z m i j acc k =
  if j >= n then k acc
  else readArr x i (\xi -> readArr y i (\yi -> readArr z i (\zi ->
       readArr x j (\xj -> readArr y j (\yj -> readArr z j (\zj ->
       readArr m i (\mi -> readArr m j (\mj ->
         let dx = xi -. xj
             dy = yi -. yj
             dz = zi -. zj
         in pairE x y z m i (j+1) (acc -. mi *. mj /. sqrtD (dx*.dx +. dy*.dy +. dz*.dz)) k))))))))
