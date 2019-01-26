module Main where

 import Prelude

 main :: IO ()
 main = do
  s <- return 1000000000 :: IO Int
  putStrLn $ fizz_buzz_string_t s

 fizz_buzz_string_t :: Int -> String
 fizz_buzz_string_t s = take 20 $ fizz_buzz_string_d s

 fizz_buzz_string_d :: Int -> String
 fizz_buzz_string_d s = drop (s - 1) $ fizz_buzz_string

 fizz_buzz_string :: String
 fizz_buzz_string = concat fizz_buzz_list

 fizz_buzz_list :: [String]
 fizz_buzz_list = map fizz_buzz [1..]

 fizz_buzz :: Int -> String
 fizz_buzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0                   = "Fizz"
  |                   n `mod` 5 == 0 = "Buzz"
  | otherwise                        = show n

 fizz_buzz_length :: Int -> Int
 fizz_buzz_length n = length $ concat $ map fizz_buzz [1..n]
