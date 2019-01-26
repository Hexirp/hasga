module Main where

 import Prelude

 main :: IO ()
 main = do
  s <- readLn :: IO Int
  putStrLn $ take 20 $ drop (s - 1) $ fizz_bizz_string

 fizz_bizz_string :: String
 fizz_bizz_string = concat $ map fizz_bizz [1..]

 fizz_bizz :: Int -> String
 fizz_bizz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0                   = "Fizz"
  |                   n `mod` 5 == 0 = "Buzz"
  | otherwise                        = show n
