module Main where
 import Prelude

 import Graphics.Gloss

 main :: IO ()
 main = display (InWindow "Hello, World!" (400, 150) (10, 10)) white picture

 picture :: Picture
 picture = Translate (-170) (-20) $ Scale 0.5 0.5 $ Text "Hello, World"
