type R = Double

velocities :: [R]
velocities = [0, 9.8, -11.4]

ts :: [R]
ts = [0, 0.1 .. 6]

yRock30 :: R -> R
yRock30 t = 30 * t - 0.5 * 9.8 * t ** 2

xs :: [R]
xs = [yRock30 t | t <- ts]

sndItem :: [a] -> a
sndItem ys = case ys of
  []     -> error "Empty list has no second element"
  (x:xs) -> if null xs
            then error "1-item list has no 2nd item"
            else head xs

sndItem0 :: [a] -> a
sndItem0 xs = if null xs
              then error "Empty list has no second element"
              else if length xs < 2
                   then error "1-item list has no 2nd item"
                   else xs !! 1

palindrome :: String -> Bool
palindrome s = if length s < 2
               then True
               else take (floor (fromIntegral (length s))) s
                 == take (floor (fromIntegral (length s))) (reverse s)

cycle' :: [a] -> [a]
cycle' l = concat $ repeat l

fact :: Integer -> Integer
fact n = product [i | i <- [1 .. n]]

expList :: R -> R -> [R]
expList x n = [(1 + x / i) ** i | i <- [1 .. n]]


