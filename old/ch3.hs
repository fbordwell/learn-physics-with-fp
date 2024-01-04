f :: Double -> Double
f x = if x <= 0
      then 0
      else x

g :: Double -> Double -> Double
g nu epsilon = sqrt (nu ** 2 - epsilon ** 2)
