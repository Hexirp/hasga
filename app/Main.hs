module Main where

 import Prelude

 import Data.Word

 import Data.Ix
 import Data.Array.IArray
 import Data.Array.Unboxed

 main :: IO ()
 main = return ()

 type GameState = UArray (Word8, Word8) Bool

 -- | Build array by function to compute elements from index
 arrayByIndex
  :: (IArray a e, Ix i)
  => (i, i) -- ^ bounds of the array: (lower, highest)
  -> (i -> e) -- ^ function to compute elements from index
  -> a i e
 arrayByIndex bd f = array bd [ (i, f i) | i <- range bd ]

 -- | Size of the field
 fieldSize :: ((Word8, Word8), (Word8, Word8))
 fieldSize = ((0, 0), (63, 63))

 -- | Initial state of the game
 newGameState :: GameState
 newGameState = arrayByIndex fieldSize f
  where
   f :: (Word8, Word8) -> Bool
   f (x, y) = undefined
