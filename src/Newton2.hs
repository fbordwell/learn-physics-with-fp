{-# OPTIONS -Wall #-}

module Newton2 where

import Graphics.Gnuplot.Simple

type R = Double

type Mass = R

type Time = R

type Position = R

type Velocity = R

type Force = R

type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b =
  sum [f t * dt | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]

type AntiDerivative = R -> (R -> R) -> (R -> R)

antiDerivative :: R -> AntiDerivative
antiDerivative dt v0 a t = v0 + integral dt a 0 t

velocityCF :: Mass -> Velocity -> [Force] -> Time -> Velocity
velocityCF m v0 fs =
  let fNet = sum fs
      a0 = fNet / m
      v t = v0 + a0 * t
   in v

-- exercise 14.1
velocityCF' :: Mass -> Velocity -> [Force] -> Time -> Velocity
velocityCF' m v0 fs t =
  let fNet = sum fs
      a0 = fNet / m
   in v0 + a0 * t

positionCF :: Mass -> Position -> Velocity -> [Force] -> Time -> Position
positionCF m x0 v0 fs =
  let fNet = sum fs
      a0 = fNet / m
      x t = x0 + v0 * t + a0 * t ** 2 / 2
   in x

-- exercise 14.3
sumF :: [R -> R] -> R -> R
sumF fs =
  let fNet t = sum [f t | f <- fs]
   in fNet

velocityFt :: R -> Mass -> Velocity -> [Time -> Force] -> Time -> Velocity
velocityFt dt m v0 fs =
  -- let fNet t = sum [f t | f <- fs]
  let fNet = sumF fs -- changed for exercise 14.3
      a t = fNet t / m
   in antiDerivative dt v0 a

positionFt :: R -> Mass -> Position -> Velocity -> [Time -> Force] -> Time -> Position
positionFt dt m x0 v0 fs = antiDerivative dt x0 (velocityFt dt m v0 fs)

pedalCoast :: Time -> Force
pedalCoast t =
  let tCycle = 20
      nComplete :: Int
      nComplete = truncate (t / tCycle)
      remainder = t - fromIntegral nComplete * tCycle
   in if remainder < 10
        then 10
        else 0

childGraph :: IO ()
childGraph =
  plotFunc
    [ Title "Child pedaling then coasting",
      XLabel "Time (s)",
      YLabel "Position of Bike (m)",
      PNG "ChildPosition.png",
      Key Nothing
    ]
    [0 .. 40 :: R]
    (positionFt 0.1 20 0 0 [pedalCoast])

customLabel :: (R, R) -> String -> Attribute
customLabel (x, y) label = Custom "label" ["\"" ++ label ++ "\"" ++ " at " ++ show x ++ "," ++ show y]

fAir ::
  R -> -- drag coefficient
  R -> -- air density
  R -> -- cross-sectinal area of object
  Velocity ->
  Force
fAir drag rho area v = -drag * rho * area * abs v * v / 2

newtonSecondV ::
  Mass ->
  [Velocity -> Force] -> -- list of force functions
  Velocity -> -- current velocity
  R -- derivative of velocity
newtonSecondV m fs v0 = sum [f v0 | f <- fs] / m

updateVelocity ::
  R -> -- time interval dt
  Mass ->
  [Velocity -> Force] -> -- list of force functions
  Velocity -> -- current velocity
  Velocity -- new velocity
updateVelocity dt m fs v0 = v0 + newtonSecondV m fs v0 * dt

-- velocity from forces that depend on velocity (e.g. air resistance)
velocityFv ::
  R -> -- time step
  Mass ->
  Velocity -> -- initial velocity v0
  [Velocity -> Force] -> -- list of force functions
  Time ->
  Velocity -- velocity function
velocityFv dt m v0 fs t =
  let numSteps = abs $ round (t / dt) -- number of time steps to get as close as possible to t
   in iterate (updateVelocity dt m fs) v0 !! numSteps

-- exercise 14.4
positionFv ::
  R -> -- time step
  Mass ->
  Position -> -- initial position x0
  Velocity -> -- initial velocity v0
  [Velocity -> Force] -> -- list of force functions
  Time ->
  Position -- position function
positionFv dt m x0 v0 fs = antiDerivative dt x0 (velocityFv dt m v0 fs)

bikeVelocity :: Time -> Velocity
-- bikeVelocity = velocityFv 1 70 0 [const 100, fAir 2 1.225 0.6]
bikeVelocity = velocityFtv 1 70 (0, 0) [constF, fAirTV] -- changed for exercise 14.5
  where
    constF (_, _) = 100
    fAirTV (_, v) = fAir 2 1.225 0.6 v

newtonSecondTV ::
  Mass ->
  [(Time, Velocity) -> Force] -> -- force funcs
  (Time, Velocity) -> -- current state
  (R, R) -- deriv of state
newtonSecondTV m fs (t, v0) =
  let fNet = sum [f (t, v0) | f <- fs]
      acc = fNet / m
   in (1, acc)

updateTV ::
  R -> -- time interval dt
  Mass ->
  [(Time, Velocity) -> Force] -> -- list of force funcs
  (Time, Velocity) -> -- current state
  (Time, Velocity) -- new state
updateTV dt m fs (t, v0) =
  let (dtdt, dvdt) = newtonSecondTV m fs (t, v0)
   in (t + dtdt * dt, v0 + dvdt * dt)

statesTV ::
  R -> -- time step
  Mass ->
  (Time, Velocity) -> -- initial state
  [(Time, Velocity) -> Force] -> -- list of force funcs
  [(Time, Velocity)]
statesTV dt m tv0 fs = iterate (updateTV dt m fs) tv0

velocityFtv ::
  R -> -- time step
  Mass ->
  (Time, Velocity) -> -- initial state
  [(Time, Velocity) -> Force] -> -- list of force functions
  Time ->
  Velocity
velocityFtv dt m tv0 fs t =
  let numSteps = abs $ round (t / dt)
   in snd $ statesTV dt m tv0 fs !! numSteps

-- exercise 14.9
positionFtv ::
  R -> -- time step
  Mass ->
  Position -> -- initial position x(0)
  Velocity -> -- initial velocity v(0)
  [(Time, Velocity) -> Force] -> -- force functions
  Time ->
  Position -- position function
positionFtv dt m x0 v0 fs = antiDerivative dt x0 (velocityFtv dt m (0, v0) fs)
