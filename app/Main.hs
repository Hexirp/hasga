{-# OPTIONS -ddump-to-file -ddump-ds -O #-}

module Main where

 import Prelude

 import Control.Exception (evaluate)
 import Control.Concurrent (threadDelay)

 import Data.List (intercalate)

 import Data.Int

 import Data.Ix
 import Data.Array.IArray
 import Data.Array.Unboxed

 import System.IO

 main :: IO ()
 main = do
  hSetBuffering stdout NoBuffering
  loop newGameState $ 4 * 1000

 loop :: GameState -> Int -> IO ()
 loop a 0 = putStrLn (viewGameState a)
 loop a i = loop (updateGameState a) (i - 1)

 -- xy-.
 -- z-w.
 -- -v-.
 -- ....
 --
 -- x = (0, 0)
 -- y = (0, 1)
 -- z = (1, 0)
 -- w = (1, 2)
 -- v = (2, 1)
 type GameState = UArray (Int8, Int8) Bool

 -- | Build array by function to compute elements from index
 arrayByIndex
  :: (IArray a e, Ix i)
  => (i, i) -- ^ bounds of the array: (lower, highest)
  -> (i -> e) -- ^ function to compute elements from index
  -> a i e
 arrayByIndex bd f = array bd [ (i, f i) | i <- range bd ]

 -- | Width of x
 xWidth :: Int8
 xWidth = 16

 -- | Width of y
 yWidth :: Int8
 yWidth = 17

 -- | Size of the field
 fieldSize :: ((Int8, Int8), (Int8, Int8))
 fieldSize = ((0, 0), (xWidth - 1, yWidth - 1))

 -- | Initial state of the game
 newGameState :: GameState
 newGameState = arrayByIndex fieldSize (uncurry f)
  where
   -- -#-
   -- --#
   -- ###
   f :: Int8 -> Int8 -> Bool
   f 0 1 = True
   f 1 2 = True
   f 2 0 = True
   f 2 1 = True
   f 2 2 = True
   f _ _ = False

 -- | Update state of the game
 updateGameState :: GameState -> GameState
 updateGameState a = arrayByIndex fieldSize (uncurry f)
  where
   f :: Int8 -> Int8 -> Bool
   f x y = let
     n0 = g (x - 1, y - 1)
     n1 = g (x - 1, y    )
     n2 = g (x - 1, y + 1)
     n3 = g (x    , y - 1)
     n4 = g (x    , y    )
     n5 = g (x    , y + 1)
     n6 = g (x + 1, y - 1)
     n7 = g (x + 1, y    )
     n8 = g (x + 1, y + 1)
     n = length $ filter id [n0, n1, n2, n3, n5, n6, n7, n8]
    in
     if n4 then n == 2 || n == 3 else n == 3
   g :: (Int8, Int8) -> Bool
   g (x, y) = a ! (x `mod` xWidth, y `mod` yWidth)

 -- | View state of the game
 viewGameState :: GameState -> String
 viewGameState a = f xv
  where
   b2c :: Bool -> Char
   b2c x = if x then '#' else '-'
   xv :: [[Bool]]
   xv = [ yv x | x <- [ 0 .. xWidth - 1] ]
   yv :: Int8 -> [Bool]
   yv x = [ a ! (x, y) | y <- [ 0 .. yWidth - 1 ] ]
   f :: [[Bool]] -> String
   f []       = ""
   f (x : xs) = g x ++ "\n" ++ f xs
   g :: [Bool] -> String
   g []       = ""
   g (x : xs) = ' ' : b2c x : g xs

 -- References:
 -- - https://github.com/Hexirp/haskell-gist/blob/f940d84bd79c2d6d9f05ad68f93152abeeed6297/Lifegame.hs
 -- - https://qiita.com/lotz/items/bdb04c771efc8919b79c
