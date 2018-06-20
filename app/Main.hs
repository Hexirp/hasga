{-# LANGUAGE OverloadedStrings #-}

module Main where
 import Prelude

 import Data.Word

 import qualified SDL

 main :: IO ()
 main = do
  SDL.initialize []
  w <- SDL.createWindow "Hello, SDL2!" SDL.defaultWindow
  SDL.showWindow w
  ws <- SDL.getWindowSurface w
  SDL.surfaceFillRect ws Nothing winColor
  SDL.updateWindowSurface w
  SDL.delay 2000
  SDL.freeSurface ws
  SDL.destroyWindow w
  SDL.quit

 winColor :: SDL.V4 Word8
 winColor = SDL.V4 maxBound maxBound maxBound maxBound
