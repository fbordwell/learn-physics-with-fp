type R = Double

type Integration = (R -> R) -- function
  -> R -- lower limit
  -> R -- upper limit
  -> R -- result

integral :: R -> Integration
integral dt f a b =
  sum [f t * dt | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]

type AntiDerivative = R -- initial value
  -> (R -> R) -- function
  -> (R -> R) -- antiderivative of function

antiDerivative :: R -> AntiDerivative
antiDerivative dt v0 a t = v0 + integral dt a 0 t

type Time = R

type Position = R

type Velocity = R

type Acceleration = R

integralN :: Int -> Integration
integralN n f a b = let dt = (b - a) / fromIntegral n
                    in integral dt f a b

integralN' :: Int -> Integration
integralN' n f a b = integral ((b - a) / fromIntegral n) f a b

yRock :: Velocity -> Time -> Position
yRock v0 t = 1 / 2 * (-9.8) * t ** 2 + v0 * t + 0

vRock :: Velocity -> Time -> Velocity
vRock v0 t = -9.8 * t + v0

greaterThanOrEq7' :: Int -> Bool
greaterThanOrEq7' n = n >= 7

exercise66 :: [a] -> Bool
exercise66 l = length l > 6
