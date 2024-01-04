-- constant
e :: Double
e = exp 1

-- function
square :: Double -> Double
square x = x ** 2

cosSq :: Double -> Double
cosSq = square . cos

cosSq' :: Double -> Double
cosSq' x = square $ cos x

f :: Double -> Double
f x = sqrt (1 + x)

y :: Double -> Double
y t = 1 / 2 * (-9.8) * t ** 2 + 30 * t + 0

v :: Double -> Double
v t = (-9.8) * t + 30

sinDeg :: Double -> Double
sinDeg x = sin $ 180 / pi * x

f' :: Double -> Double
f' x = sqrt x ** 3

g :: Double -> Double
g y = exp y + 8 ** y

h :: Double -> Double
h x = 1 / sqrt ((x - 5) ** 2 + 16)

gamma :: Double -> Double
gamma beta = 1 / sqrt (1 - beta ** 2)
