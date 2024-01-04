{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mechanics3D where

import qualified Graphics.Gloss as G
import Graphics.Gnuplot.Simple
import Mechanics1D
import SimpleVec
import SpatialMath
  ( Euler (..),
    V3 (..),
  )
import qualified Vis as V

data ParticleState = ParticleState
  { mass :: R,
    charge :: R,
    time :: R,
    posVec :: Vec,
    velocity :: Vec
  }
  deriving (Show)

defaultParticleState :: ParticleState
defaultParticleState =
  ParticleState
    { mass = 1,
      charge = 0,
      time = 0,
      posVec = zeroV,
      velocity = zeroV
    }

-- a 2-kg rock with no net charge, at the origin, moving with velocity (3i + 4k) m/s
rockState :: ParticleState
rockState =
  defaultParticleState
    { mass = 2, -- kg
      velocity = 3 *^ iHat ^+^ 4 *^ kHat -- m/s
    }

type OneBodyForce = ParticleState -> Vec

data DParticleState = DParticleState
  { dmdt :: R,
    dqdt :: R,
    dtdt :: R,
    drdt :: Vec,
    dvdt :: Vec
  }
  deriving (Show)

newtonSecondPS ::
  [OneBodyForce] ->
  ParticleState ->
  DParticleState
newtonSecondPS fs st =
  let fNet = sumV [f st | f <- fs]
      m = mass st
      v = velocity st
      acc = fNet ^/ m
   in DParticleState
        { dmdt = 0,
          dqdt = 0,
          dtdt = 1,
          drdt = v,
          dvdt = acc
        }

-- z direction is toward the sky
-- assumes SI units
earthSurfaceGravity :: OneBodyForce
earthSurfaceGravity st =
  let g = 9.80665 -- m/s^2
   in (-mass st * g) *^ kHat

-- origin is at center of sun
-- assumes SI units
sunGravity :: OneBodyForce
sunGravity (ParticleState m _q _t r _v) =
  let bigG = 6.67408e-11 -- N m^2/kg^2
      sunMass = 1.98848e30 -- kg
   in (-bigG * sunMass * m) *^ r ^/ magnitude r ** 3

airResistance ::
  R -> -- drag coefficient
  R -> -- air density
  R -> -- cross-sectional area of object
  OneBodyForce
airResistance drag rho area (ParticleState _m _q _t _r v) = (-0.5 * drag * rho * area * magnitude v) *^ v

windForce ::
  Vec -> -- wind velocity
  R -> -- drag coefficient
  R -> -- air density
  R -> -- cross-sectional area of object
  OneBodyForce
windForce vWind drag rho area (ParticleState _m _q _t _r v) =
  let vRel = v ^-^ vWind
   in (-0.5 * drag * rho * area * magnitude vRel) *^ vRel

uniformLorentzForce ::
  Vec -> -- E
  Vec -> -- B
  OneBodyForce
uniformLorentzForce vE vB (ParticleState _m q _t _r v) = q *^ (vE ^+^ v >< vB)

eulerCromerPS ::
  TimeStep -> -- dt for stepping
  NumericalMethod ParticleState DParticleState
eulerCromerPS dt deriv st =
  let t = time st
      r = posVec st
      v = velocity st
      dst = deriv st
      acc = dvdt dst
      v' = v ^+^ acc ^* dt
   in st
        { time = t + dt,
          posVec = r ^+^ v' ^* dt,
          velocity = v ^+^ acc ^* dt
        }

instance RealVectorSpace DParticleState where
  dst1 +++ dst2 =
    DParticleState
      { dmdt = dmdt dst1 + dmdt dst2,
        dqdt = dqdt dst1 + dqdt dst2,
        dtdt = dtdt dst1 + dtdt dst2,
        drdt = drdt dst1 ^+^ drdt dst2,
        dvdt = dvdt dst1 ^+^ dvdt dst2
      }
  scale w dst =
    DParticleState
      { dmdt = w * dmdt dst,
        dqdt = w * dqdt dst,
        dtdt = w * dtdt dst,
        drdt = w *^ drdt dst,
        dvdt = w *^ dvdt dst
      }

instance Diff ParticleState DParticleState where
  shift dt dps (ParticleState m q t r v) =
    ParticleState
      (m + dmdt dps * dt)
      (q + dqdt dps * dt)
      (t + dtdt dps * dt)
      (r ^+^ drdt dps ^* dt)
      (v ^+^ dvdt dps ^* dt)

statesPS ::
  NumericalMethod ParticleState DParticleState ->
  [OneBodyForce] -> -- list of force funcs
  ParticleState ->
  [ParticleState] -- evolver
statesPS method = iterate . method . newtonSecondPS

updatePS ::
  NumericalMethod ParticleState DParticleState ->
  [OneBodyForce] ->
  ParticleState ->
  ParticleState
updatePS method = method . newtonSecondPS

positionPS ::
  NumericalMethod ParticleState DParticleState ->
  [OneBodyForce] -> -- list of force funcs
  ParticleState -> -- initial state
  Time ->
  PosVec -- position function
positionPS method fs st t =
  let states = statesPS method fs st
      dt = time (states !! 1) - time (head states)
      numSteps = abs $ round (t / dt)
      st1 = solver method (newtonSecondPS fs) st !! numSteps
   in posVec st1

simulateGloss ::
  R -> -- time-scale factor
  Int -> -- animation rate
  s -> -- initial state
  (s -> G.Picture) ->
  (TimeStep -> s -> s) ->
  IO ()
simulateGloss tsFactor rate initialState picFunc updateFunc =
  G.simulate
    (G.InWindow "" (1000, 750) (10, 10))
    G.black
    rate
    initialState
    picFunc
    (\_ -> updateFunc . (* tsFactor) . realToFrac)

simulateVis ::
  (HasTime s) =>
  R -> -- time-scale factor
  Int -> -- animation rate
  s -> -- initial state
  (s -> V.VisObject R) ->
  (TimeStep -> s -> s) ->
  IO ()
simulateVis tsFactor rate initialState picFunc updateFunc =
  let visUpdateFunc ta st =
        let dtp = tsFactor * realToFrac ta - timeOf st
         in updateFunc dtp st
   in V.simulate
        V.defaultOpts
        (1 / fromIntegral rate)
        initialState
        (orient . picFunc)
        visUpdateFunc

v3FromVec :: Vec -> V3 R
v3FromVec v = V3 x y z
  where
    x = xComp v
    y = yComp v
    z = zComp v

orient :: V.VisObject R -> V.VisObject R
orient = V.RotEulerDeg (Euler 270 180 0)

class HasTime s where
  timeOf :: s -> Time

instance HasTime ParticleState where
  timeOf = time

-- exercise 16.2
constantForce :: Vec -> OneBodyForce
constantForce = const

tenNewtoniHatForce :: OneBodyForce
tenNewtoniHatForce = constantForce (10 *^ iHat)

-- exercise 16.3
moonSurfaceGravity :: OneBodyForce
moonSurfaceGravity st =
  let g = 1.62
   in (-mass st * g) *^ kHat

-- exercise 16.4
earthGravity :: OneBodyForce
earthGravity (ParticleState m _ _ r _) =
  let bigG = 6.67408e-11 -- N m^2/kg^2
      earthMass = 5.972e24 -- kg
   in (-bigG * earthMass * m) *^ r ^/ magnitude r ** 3

-- exercise 16.6
tvyPair :: ParticleState -> (R, R)
tvyPair (ParticleState _ _ t _ v) = (t, yComp v)

tvyPairs :: [ParticleState] -> [(R, R)]
tvyPairs = map tvyPair

-- exercise 16.7
tle1yr :: ParticleState -> Bool
tle1yr st = time st < (365 * 24 * 60 * 60)

-- exercise 16.8
stateFunc :: [ParticleState] -> Time -> ParticleState
stateFunc sts t =
  let t0 = time $ head sts
      t1 = time $ sts !! 2
      dt = t1 - t0
      numSteps = round (t / dt)
   in sts !! numSteps

-- exercise 16.9
airResAtAltitude ::
  R -> -- drag coefficient
  R -> -- air density at sea level
  R -> -- corss-sectional area of object
  OneBodyForce
-- airResAtAltitude drag rho0 area (ParticleState _ _ _ r v) =
airResAtAltitude drag rho0 area st =
  let h0 = 8500
      h = zComp (posVec st)
      rho = rho0 * (exp 1 ** (h / h0))
   in airResistance drag rho area st

projectileRangeComparison :: R -> R -> (R, R, R)
projectileRangeComparison v0 thetaDeg =
  let vx0 = v0 * cos (thetaDeg / 180 * pi)
      vz0 = v0 * sin (thetaDeg / 180 * pi)
      drag = 1
      ballRadius = 0.05 -- meters
      area = pi * ballRadius ** 2
      airDensity = 1.225 -- kg/m^3 @ sea level
      leadDensity = 11342 -- kg/m^3
      m = leadDensity * 4 * pi * ballRadius ** 3 / 3
      stateInitial = ParticleState {mass = m, charge = 0, time = 0, posVec = zeroV, velocity = vec vx0 0 vz0}
      aboveSeaLevel :: ParticleState -> Bool
      aboveSeaLevel st = zComp (posVec st) >= 0
      range :: [ParticleState] -> R
      range = xComp . posVec . last . takeWhile aboveSeaLevel
      method = rungeKutta4 0.01
      forcesNoAir = [earthSurfaceGravity]
      forcesConstAir = [earthSurfaceGravity, airResistance drag airDensity area]
      forcesVarAir = [earthSurfaceGravity, airResAtAltitude drag airDensity area]
      rangeNoAir = range $ statesPS method forcesNoAir stateInitial
      rangeConstAir = range $ statesPS method forcesConstAir stateInitial
      rangeVarAir = range $ statesPS method forcesVarAir stateInitial
   in (rangeNoAir, rangeConstAir, rangeVarAir)

-- exercise 16.10
ballFromTenMeters :: [ParticleState]
ballFromTenMeters =
  let initialState =
        ParticleState
          { mass = 1,
            charge = 0,
            time = 0,
            posVec = vec 0 0 10,
            velocity = zeroV
          }
      dt = 0.01
      forces = [earthSurfaceGravity]
   in takeWhile (\st -> zComp (posVec st) >= 0) $ statesPS (eulerCromerPS dt) forces initialState

halleyUpdate :: TimeStep -> ParticleState -> ParticleState
halleyUpdate dt = updatePS (eulerCromerPS dt) [sunGravity]

halleyInitial :: ParticleState
halleyInitial =
  ParticleState
    { mass = 2.2e14, -- kg
      charge = 0,
      time = 0,
      posVec = 8.766e10 *^ iHat, -- m
      velocity = 54569 *^ jHat -- m/s
    }

disk :: Float -> G.Picture
disk radius = G.ThickCircle (radius / 2) radius

baseballForces :: [OneBodyForce]
baseballForces =
  let area = pi * (0.074 / 2) ** 2
   in [earthSurfaceGravity, airResistance 0.3 1.225 area]

baseballTrajectory ::
  R -> -- time step
  R -> -- initial speed
  R -> -- launch angle in degrees
  [(R, R)] -- (y,z) pairs
baseballTrajectory dt v0 thetaDeg =
  let thetaRad = thetaDeg * pi / 180
      vy0 = v0 * cos thetaRad
      vz0 = v0 * sin thetaRad
      initialState =
        ParticleState
          { mass = 0.145,
            charge = 0,
            time = 0,
            posVec = zeroV,
            velocity = vec 0 vy0 vz0
          }
   in trajectory $ zGE0 $ statesPS (eulerCromerPS dt) baseballForces initialState

zGE0 :: [ParticleState] -> [ParticleState]
zGE0 = takeWhile (\(ParticleState _ _ _ r _) -> zComp r >= 0)

trajectory :: [ParticleState] -> [(R, R)]
trajectory sts = [(yComp r, zComp r) | (ParticleState _ _ _ r _) <- sts]

baseballRange ::
  R -> -- time step
  R -> -- initial speed
  R -> -- launch angle in degrees
  R -- range
baseballRange dt v0 thetaDeg =
  let (y, _) = last $ baseballTrajectory dt v0 thetaDeg
   in y

baseballRangeGraph :: IO ()
baseballRangeGraph =
  plotFunc
    [ Title "Range for baseball hit at 45 m/s",
      XLabel "Angle above horizontal (degrees)",
      YLabel "Horizontal Range (m)",
      PNG "baseballrange.png",
      Key Nothing
    ]
    [10, 11 .. 80]
    $ baseballRange 0.01 45

bestAngle :: (R, R)
bestAngle = maximum [(baseballRange 0.01 45 thetaDeg, thetaDeg) | thetaDeg <- [30, 31 .. 60]]

protonUpdate :: TimeStep -> ParticleState -> ParticleState
protonUpdate dt = updatePS (rungeKutta4 dt) [uniformLorentzForce zeroV (3e-8 *^ kHat)]

protonPicture :: ParticleState -> V.VisObject R
protonPicture st =
  let r0 = v3FromVec (posVec st)
   in V.Trans r0 (V.Sphere 0.1 V.Solid V.red)

relativityPS :: [OneBodyForce] -> ParticleState -> DParticleState -- a differential equation
relativityPS fs st =
  let fNet = sumV [f st | f <- fs]
      c = 299792458
      m = mass st
      v = velocity st
      u = v ^/ c
      acc = sqrt (1 - u <.> u) *^ (fNet ^-^ (fNet <.> u) *^ u) ^/ m
   in DParticleState {dmdt = 0, dqdt = 0, dtdt = 1, drdt = v, dvdt = acc}


