module Main where

 import Prelude

 import Data.Word

 import Data.Ix
 import Data.Array
 import Data.Array.IArray
 import Data.Array.Unboxed

 main :: IO ()
 main = return ()

 type GameState = UArray (Word8, Word8) Bool

 newGameState :: GameState
 newGameState = undefined
