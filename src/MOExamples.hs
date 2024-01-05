module MOExamples where

import qualified Graphics.Gloss as G
import Graphics.Gnuplot.Simple
import Mechanics1D hiding (Force)
import Mechanics3D
import MultipleObjects hiding (systemKE)
import Newton2 (customLabel)
import SimpleVec

twoSpringsForces :: [Force]
twoSpringsForces =
  [ ExternalForce 0 (fixedLinearSpring 100 0.5 zeroV),
    InternalForce 0 1 (linearSpring 100 0.5),
    ExternalForce 0 earthSurfaceGravity,
    ExternalForce 1 earthSurfaceGravity
  ]

twoSpringsInitial :: MultiParticleState
twoSpringsInitial =
  MPS
    [ defaultParticleState
        { mass = 2,
          posVec = 0.4 *^ jHat ^-^ 0.3 *^ kHat
        },
      defaultParticleState
        { mass = 3,
          posVec = 0.4 *^ jHat ^-^ 0.8 *^ kHat
        }
    ]

twoSpringsUpdate ::
  TimeStep ->
  MultiParticleState -> -- old state
  MultiParticleState -- new state
twoSpringsUpdate dt = updateMPS (eulerCromerMPS dt) twoSpringsForces

kineticEnergy :: ParticleState -> R
kineticEnergy (ParticleState m _ _ _ v) = 1 / 2 * m * magnitude v ** 2

systemKE :: MultiParticleState -> R
systemKE (MPS sts) = sum $ map kineticEnergy sts

linearSpringPE ::
  R -> -- spring constant
  R -> -- equilibrium length
  ParticleState -> -- state of particle at one end of spring
  ParticleState -> -- state of particle at other end of spring
  R -- potential energy of the spring
linearSpringPE k re (ParticleState _ _ _ r1 _) (ParticleState _ _ _ r2 _) =
  let r21mag = magnitude $ r2 ^-^ r1
   in k * (r21mag - re) ** 2 / 2

earthSurfaceGravityPE :: ParticleState -> R
earthSurfaceGravityPE (ParticleState m _ _ r _) = m * 9.80665 * zComp r

twoSpringsPE :: MultiParticleState -> R
twoSpringsPE (MPS sts) =
  linearSpringPE 100 0.5 defaultParticleState (head sts)
    + linearSpringPE 100 0.5 (head sts) (sts !! 1)
    + earthSurfaceGravityPE (head sts)
    + earthSurfaceGravityPE (sts !! 1)

twoSpringsME :: MultiParticleState -> R
twoSpringsME mpst = systemKE mpst + twoSpringsPE mpst

billiardForces :: R -> [Force]
billiardForces k = [InternalForce 0 1 (billiardForce k (2 * ballRadius))]

ballRadius :: R
ballRadius = 0.03 -- 6cm diameter = 0.03m radius

billiardDiffEq :: R -> MultiParticleState -> DMultiParticleState
billiardDiffEq k = newtonSecondMPS $ billiardForces k

billiardUpdate ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  R -> -- k
  TimeStep -> -- dt
  MultiParticleState ->
  MultiParticleState
billiardUpdate nMethod k dt = updateMPS (nMethod dt) (billiardForces k)

billiardEvolver ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  R -> -- k
  TimeStep -> -- dt
  MultiParticleState ->
  [MultiParticleState]
billiardEvolver nMethod k dt = statesMPS (nMethod dt) (billiardForces k)

billiardInitial :: MultiParticleState
billiardInitial =
  let ballMass = 0.160 -- 160g
   in MPS
        [ defaultParticleState
            { mass = ballMass,
              posVec = zeroV,
              velocity = 0.2 *^ iHat
            },
          defaultParticleState
            { mass = ballMass,
              posVec = iHat ^+^ 0.02 *^ jHat,
              velocity = zeroV
            }
        ]

billiardStates ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  R -> -- k
  TimeStep -> -- dt
  [MultiParticleState]
billiardStates nMethod k dt = statesMPS (nMethod dt) (billiardForces k) billiardInitial

billiardStatesFinite ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  R -> -- k
  TimeStep -> -- dt
  [MultiParticleState]
billiardStatesFinite nMethod k dt = takeWhile (\st -> timeOf st <= 10) (billiardStates nMethod k dt)

momentum :: ParticleState -> Vec
momentum (ParticleState m _ _ _ v) = m *^ v

systemP :: MultiParticleState -> Vec
systemP (MPS sts) = sumV $ map momentum sts

percentChangePMag :: [MultiParticleState] -> R
percentChangePMag mpsts =
  let p0 = systemP (head mpsts)
      p1 = systemP (last mpsts)
   in 100 * magnitude (p1 ^-^ p0) / magnitude p0

sigFigs :: Int -> R -> Float
sigFigs n x =
  let expon :: Int
      expon = floor (logBase 10 x) - n + 1
      toInt :: R -> Int
      toInt = round
   in (10 ^^ expon *) $ fromIntegral $ toInt (10 ^^ (-expon) * x)

data Justification = LJ | RJ deriving (Show)

data Table a = Table Justification [[a]]

instance (Show a) => Show (Table a) where
  show (Table j xss) =
    let pairWithLength x = let str = show x in (str, length str)
        pairss = map (map pairWithLength) xss
        maxLength = maximum $ map (maximum . map snd) pairss
        showPair (str, len) = case j of
          LJ -> str ++ replicate (maxLength + 1 - len) ' '
          RJ -> replicate (maxLength + 1 - len) ' ' ++ str
        showLine pairs = concatMap showPair pairs ++ "\n"
     in init $ concatMap showLine pairss

pTable ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  [R] -> -- ks
  [TimeStep] -> -- dts
  Table Float
pTable nMethod ks dts = Table LJ [[sigFigs 2 $ percentChangePMag (billiardStatesFinite nMethod k dt) | dt <- dts] | k <- ks]

pTableEu ::
  [R] -> -- ks
  [TimeStep] -> -- dts
  Table Float
pTableEu = pTable euler

systemKEWithTime :: IO ()
systemKEWithTime =
  let timeKEPairsEC = map (\mpst -> (timeOf mpst, systemKE mpst)) (billiardStatesFinite eulerCromerMPS 30 0.03)
      timeKEPairsRK4 = map (\mpst -> (timeOf mpst, systemKE mpst)) (billiardStatesFinite rungeKutta4 30 0.03)
   in plotPaths
        [ Key Nothing,
          Title "System Kinetic Energy versus Time",
          XLabel "Time (s)",
          YLabel "System Kinetic Energy (J)",
          XRange (4, 6),
          PNG "SystemKE.png",
          customLabel (4.1, 0.0026) "dt = 0.03 s",
          customLabel (4.1, 0.0025) "k = 30 N/m",
          customLabel (5.4, 0.00329) "Euler-Cromer",
          customLabel (5.4, 0.00309) "Runge-Kutta 4"
        ]
        [timeKEPairsEC, timeKEPairsRK4]

percentChangeKE :: [MultiParticleState] -> R
percentChangeKE mpsts =
  let ke0 = systemKE $ head mpsts
      ke1 = systemKE $ last mpsts
   in 100 * (ke1 - ke0) / ke0

tenths :: R -> Float
tenths =
  let toInt :: R -> Int
      toInt = round
   in (/ 10) . fromIntegral . toInt . (* 10)

keTable ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  [R] -> -- ks
  [TimeStep] -> -- dts
  Table Float
keTable nMethod ks dts = Table RJ [[tenths $ percentChangeKE (billiardStatesFinite nMethod k dt) | dt <- dts] | k <- ks]

contactSteps :: [MultiParticleState] -> Int
contactSteps = length . takeWhile inContact . dropWhile (not . inContact)

inContact :: MultiParticleState -> Bool
inContact (MPS sts) =
  let r = magnitude $ posVec (head sts) ^-^ posVec (sts !! 1)
   in r < 2 * ballRadius

contactTable ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  [R] -> -- ks
  [TimeStep] -> -- dts
  Table Int
contactTable nMethod ks dts = Table RJ [[contactSteps (billiardStatesFinite nMethod k dt) | dt <- dts] | k <- ks]

closest :: [MultiParticleState] -> R
closest = minimum . map separation

separation :: MultiParticleState -> R
separation (MPS sts) = magnitude $ posVec (head sts) ^-^ posVec (sts !! 1)

closestTable ::
  (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState) ->
  [R] -> -- ks
  [TimeStep] -> -- dts
  Table Float
closestTable nMethod ks dts = Table RJ [[tenths $ (100 *) $ closest (billiardStatesFinite nMethod k dt) | dt <- dts] | k <- ks]

billiardPicture :: MultiParticleState -> G.Picture
billiardPicture (MPS sts) =
  G.scale ppm ppm $ G.pictures $ map place sts
  where
    ppm = 300 -- pixels per meter
    place st = G.translate (xSt st) (ySt st) blueBall
    xSt = realToFrac . xComp . posVec
    ySt = realToFrac . yComp . posVec
    blueBall = G.Color G.blue $ disk $ realToFrac ballRadius

forcesString :: [Force]
forcesString =
  [ ExternalForce 0 (fixedLinearSpring 5384 0 (vec 0 0 0)),
    ExternalForce 63 (fixedLinearSpring 5384 0 (vec 0.65 0 0))
  ]
    ++ [InternalForce n (n + 1) (linearSpring 5384 0) | n <- [0 .. 62]]

stringUpdate ::
  TimeStep ->
  MultiParticleState -> -- old state
  MultiParticleState -- new state
stringUpdate dt = updateMPS (rungeKutta4 dt) forcesString

stringInitialOvertone :: Int -> MultiParticleState
stringInitialOvertone n =
  MPS
    [ defaultParticleState
        { mass = 0.8293e-3 * 0.65 / 64,
          posVec = x *^ iHat ^+^ y *^ jHat,
          velocity = zeroV
        }
      | x <- [0.01, 0.02 .. 0.64],
        let y = 0.005 * sin (fromIntegral n * pi * x / 0.65)
    ]

stringInitialPluck :: MultiParticleState
stringInitialPluck =
  MPS
    [ defaultParticleState
        { mass = 0.8293e-3 * 0.65 / 64,
          posVec = x *^ iHat ^+^ y *^ jHat,
          velocity = zeroV
        }
      | x <- [0.01, 0.02 .. 0.64],
        let y = pluckEq x
    ]
  where
    pluckEq :: R -> R
    pluckEq x
      | x <= 0.51 = 0.005 / (0.51 - 0.00) * (x - 0.00)
      | otherwise = 0.005 / (0.51 - 0.65) * (x - 0.65)

-- exercise 20.2
billiardPictureColors :: MultiParticleState -> G.Picture
billiardPictureColors (MPS sts) =
  G.scale ppm ppm $ G.pictures $ zipWith place sts (cycle [G.blue, G.red])
  where
    ppm = 300 -- pixels per meteR
    place st color = G.translate (xSt st) (ySt st) $ coloredBall color
    xSt = realToFrac . xComp . posVec
    ySt = realToFrac . yComp . posVec
    coloredBall :: G.Color -> G.Picture
    coloredBall color = G.Color color $ disk $ realToFrac ballRadius
