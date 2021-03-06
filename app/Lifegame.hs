module Lifegame where

 import Prelude

 import Control.Exception (evaluate)
 import Control.Concurrent (threadDelay) -- 速度をいじるときに使う

 import Data.List (intercalate)

 import Data.Int

 import Data.Ix
 import Data.Array.IArray
 import Data.Array.Unboxed

 import System.IO

 main :: IO ()
 main = do
  hSetBuffering stdout NoBuffering
  loop newGameState $ 40 * 1000

 loop :: GameState -> Int -> IO ()
 loop a 0 = evaluate a >>= \a' -> putStrLn (viewGameState a)
 loop a i = evaluate a >>= \a' -> loop (updateGameState a') (i - 1)

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
 type GameState = UArray (Int, Int) Int

 -- | Build array by function to compute elements from index
 arrayByIndex
  :: (IArray a e, Ix i)
  => (i, i) -- ^ bounds of the array: (lower, highest)
  -> (i -> e) -- ^ function to compute elements from index
  -> a i e
 arrayByIndex bd f = array bd [ (i, f i) | i <- range bd ]

 -- | Width of x
 xWidth :: Int
 xWidth = 16

 -- | Width of y
 yWidth :: Int
 yWidth = 17

 -- | Size of the field
 fieldSize :: ((Int, Int), (Int, Int))
 fieldSize = ((0, 0), (xWidth - 1, yWidth - 1))

 -- | Initial state of the game
 newGameState :: GameState
 newGameState = arrayByIndex fieldSize (uncurry f)
  where
   -- -#-
   -- --#
   -- ###
   f :: Int -> Int -> Int
   f 0 1 = 1
   f 1 2 = 1
   f 2 0 = 1
   f 2 1 = 1
   f 2 2 = 1
   f _ _ = 0

 -- | Update state of the game
 updateGameState :: GameState -> GameState
 updateGameState a = arrayByIndex fieldSize (uncurry f)
  where
   b2i :: Bool -> Int
   b2i False = 0
   b2i True  = 1
   f :: Int -> Int -> Int
   f x y = let
     xn = x - 1
     xz = x
     xp = x + 1
     yn = y - 1
     yz = y
     yp = y + 1
     n0 = g (xn, yn)
     n1 = g (xn, yz)
     n2 = g (xn, yp)
     n3 = g (xz, yn)
     n4 = g (xz, yz)
     n5 = g (xz, yp)
     n6 = g (xp, yn)
     n7 = g (xp, yz)
     n8 = g (xp, yp)
     n = n0 + n1 + n2 + n3 + n5 + n6 + n7 + n8
    in
     if n4 == 0 then b2i (n == 3) else b2i (n == 2 || n == 3)
   g :: (Int, Int) -> Int
   g (x, y) = a ! (x `mod` xWidth, y `mod` yWidth)

 -- | View state of the game
 viewGameState :: GameState -> String
 viewGameState a = f xv
  where
   i2c :: Int -> Char
   i2c x = if x == 0 then '-' else '#'
   xv :: [[Int]]
   xv = [ yv x | x <- [ 0 .. xWidth - 1] ]
   yv :: Int -> [Int]
   yv x = [ a ! (x, y) | y <- [ 0 .. yWidth - 1 ] ]
   f :: [[Int]] -> String
   f []       = ""
   f (x : xs) = g x ++ "\n" ++ f xs
   g :: [Int] -> String
   g []       = ""
   g (x : xs) = ' ' : i2c x : g xs

 -- References:
 -- - https://github.com/Hexirp/haskell-gist/blob/f940d84bd79c2d6d9f05ad68f93152abeeed6297/Lifegame.hs
 -- - https://qiita.com/lotz/items/bdb04c771efc8919b79c
