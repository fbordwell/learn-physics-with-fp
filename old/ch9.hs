type R = Double

type Integration =
  (R -> R) -> -- function
  R -> -- lower limit
  R -> -- upper limit
  R -- result

oneStep ::
  R -> -- time step
  (R -> R) -> -- function to integrate
  (R, R) -> -- current (t, y)
  (R, R) -- updated (t, y)
oneStep dt f (t, y) =
  let t' = t + dt
      y' = y + f t * dt
   in (t', y')

integral' :: R -> Integration
integral' dt f a b =
  snd $
    head $
      dropWhile (\(t, _) -> t < b) $
        iterate (oneStep dt f) (a + dt / 2, 0)

polarToCart :: (R, R) -> (R, R)
polarToCart (r, theta) = (r * cos theta, r * sin theta)

headSafe :: [a] -> Maybe a
headSafe a =
  if null a
    then Nothing
    else Just $ head a

maybeToList :: Maybe a -> [a]
maybeToList a = case a of
  Nothing -> []
  Just x -> [x]

zip' :: ([a], [b]) -> [(a, b)]
zip' (a, b) = zip a b

tvPairs :: [(R, R)]
tvPairs = iterate tvUpdate (0, 0)

tvUpdate :: (R, R) -> (R, R)
tvUpdate (t, v) = (t + 1, v + 5)

fibonacci :: [Int]
fibonacci = map snd (iterate fibHelper (0, 1))

fibHelper :: (Int, Int) -> (Int, Int)
fibHelper (x, y) = (y, x + y)

pick13 :: [(R, R, R)] -> [(R, R)]
pick13 triples = [(x1, x3) | (x1, _, x3) <- triples]

toTriple :: ((a, b), c) -> (a, b, c)
toTriple ((a, b), c) = (a, b, c)
