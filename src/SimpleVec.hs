{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module SimpleVec where

infixl 6 ^+^

infixl 6 ^-^

infixr 7 *^

infixl 7 ^*

infixr 7 ^/

infixr 7 <.>

infixl 7 ><

type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

type R = Double

type Time = R

type PosVec = Vec

type Velocity = Vec

type Acceleration = Vec

type VecDerivative = (R -> Vec) -> R -> Vec

vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt / 2) ^-^ v (t - dt / 2)) ^/ dt

velFromPos :: R -> (Time -> PosVec) -> (Time -> Velocity)
velFromPos = vecDerivative

accFromVel :: R -> (Time -> Velocity) -> (Time -> Acceleration)
accFromVel = vecDerivative

posFromVel :: R -> VecIntegral
posFromVel = vecIntegral

velFromAcc :: R -> VecIntegral
velFromAcc = vecIntegral

positionCV :: PosVec -> Velocity -> Time -> PosVec
positionCV r0 v0 t = v0 ^* t ^+^ r0

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 ^* t ^+^ v0

positionCA :: PosVec -> Velocity -> Acceleration -> Time -> PosVec
positionCA r0 v0 a0 t = 0.5 *^ t ** 2 *^ a0 ^+^ v0 ^* t ^+^ r0

aParallel :: Vec -> Vec -> Vec
aParallel v a =
  let vHat = v ^/ magnitude v
   in (vHat <.> a) *^ vHat

aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a

radiusOfCurvature :: Vec -> Vec -> R
radiusOfCurvature v a = (v <.> v) / magnitude (aPerp v a)

projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 v0 = positionCA r0 v0 (9.81 *^ negateV kHat)

data Mass = Mass R
  deriving (Eq, Show)

data Grade = Grade String Int
  deriving (Eq, Show)

grades :: [Grade]
grades =
  [Grade "Albert Einstein" 89, Grade "Isaac Newton" 95, Grade "Alan Turing" 91]

data GradeRecord = GradeRecord {name :: String, grade :: Int}
  deriving (Eq, Show)

data MyBool
  = MyFalse
  | MyTrue
  deriving (Eq, Show)

data MyMaybe a
  = MyNothing
  | MyJust a
  deriving (Eq, Show)

data Vec = Vec {xComp :: R, yComp :: R, zComp :: R}
  deriving (Eq)

instance Show Vec where
  show (Vec x y z) =
    "vec " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z

showDouble :: R -> String
showDouble x
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x

vec :: R -> R -> R -> Vec
vec = Vec

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax + bx) (ay + by) (az + bz)

(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax - bx) (ay - by) (az - bz)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c * ax) (c * ay) (c * az)

(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c * ax) (c * ay) (c * az)

(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax * bx + ay * by + az * bz

(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz =
  Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax / c) (ay / c) (az / c)

magnitude :: Vec -> R
magnitude v = sqrt (v <.> v)

-- exercises
type VecIntegral = (R -> Vec) -> R -> R -> Vec

vecIntegral :: R -> VecIntegral
vecIntegral dt v a b =
  sumV [v t ^* dt | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]

maxHeight :: PosVec -> Velocity -> R
maxHeight x v =
  let g = -9.8
      tmax = zComp v / g
   in zComp (x ^+^ v ^* tmax) - 1 / 2 * g * tmax ** 2

speedCA :: Velocity -> Acceleration -> Time -> R
speedCA v a t = magnitude $ v ^+^ a ^* t