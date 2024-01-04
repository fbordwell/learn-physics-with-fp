type R = Double

type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

type Time = R

type Position = R

carPosition :: Time -> Position
carPosition t = cos t

type Velocity = R

carVelocity :: Time -> Velocity
carVelocity = derivative 0.01 carPosition

type Acceleration = R

accFromVel :: R                      -- dt
           -> (Time -> Velocity)     -- velocity function
           -> (Time -> Acceleration) -- acceleration function
accFromVel = derivative

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 * t + v0

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a0 t = a0 * t ** 2 / 2 + v0 * t + x0

fx :: R -> R
fx x = x ** 3

df :: R -> R -> R
df d x = derivative d fx x

dcos :: R -> R -> R
dcos a t = derivative a cos t

pos1 :: Time -> Position
pos1 t = if t < 0
         then 0
         else 5 * t ** 2

vel1Analytic :: Time -> Velocity
vel1Analytic t = if t < 0
                 then 0
                 else 10 * t

acc1Analytic :: Time -> Acceleration
acc1Analytic t = if t < 0
                 then 0
                 else 10

vel1Numerical :: Time -> Velocity
vel1Numerical = derivative 0.01 pos1

acc1Numerical :: Time -> Velocity
acc1Numerical = derivative 0.01 vel1Numerical
