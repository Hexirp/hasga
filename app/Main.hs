module Main where

 import Prelude

 main :: IO ()
 main = do
  s <- readLn :: IO Int
  putStrLn $ take 20 $ drop (s - 1) $ fizz_buzz_string

 fizz_buzz_string :: String
 fizz_buzz_string = concat $ map fizz_buzz [1..]

 fizz_buzz :: Int -> String
 fizz_buzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0                   = "Fizz"
  |                   n `mod` 5 == 0 = "Buzz"
  | otherwise                        = show n
