module Main where
 import Prelude

 import Graphics.Gloss

 main :: IO ()
 main = simulate di white 0 vi (\_ _ m -> m + 0.05)

 di :: Display
 di = InWindow "I see you." (400, 400) (200, 200)

 vi :: Float -> Picture
 vi m = Rotate m $ Polygon [
  (0, 0),
  (0, 10),
  (10, 10),
  (10, 0)]
