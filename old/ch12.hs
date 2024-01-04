{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

axes :: Picture
axes =
  Pictures
    [Color red $ Line [(0, 0), (100, 0)], Color green $ Line [(0, 0), (0, 100)]]

blueCircle :: Picture
blueCircle = Color blue (Circle 100)

projectileMotion :: Float -> Picture
projectileMotion t = Translate (xDisk t) (yDisk t) redDisk

xDisk :: Float -> Float
xDisk t = 40 * t

yDisk :: Float -> Float
yDisk t = 80 * t - 4.9 * t ** 2

wholePicture :: Picture
wholePicture =
  Pictures
    [Translate (-120) 0 blueCircle, Translate 120 0 redDisk]

rate :: Int
rate = 30

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 25)

type State = (Float, Float)

initialState :: State
initialState = (0, 0)

displayFunc :: State -> Picture
displayFunc (x, y) = Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt (x, y) = (x + 10 * dt, y - 5 * dt)

main :: IO ()
main =
  simulate displayMode black rate initialState displayFunc (const updateFunc)
