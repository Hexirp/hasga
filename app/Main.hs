module Main where
 import Prelude

 import Graphics.Gloss

 main :: IO ()
 main = simulate di white sp 0 vi (\_ _ m -> m + 0.05)

 di :: Display
 di = InWindow "I see you." (400, 400) (200, 200)

 sp :: Int
 sp = 30

 vi :: Float -> Picture
 vi m = Rotate m $ Polygon [
  (0, 0),
  (0, 50),
  (50, 50),
  (50, 0)]
