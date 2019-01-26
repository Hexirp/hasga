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

 -- approx. (32/15) * x + (7/15) * x * log_10 x
 fizz_buzz_length :: Int -> Int
 fizz_buzz_length n = length $ concat $ map fizz_buzz [1..n]

 -- 1: 12Fizz4BuzzFizz78Fizz
 -- 2: Buzz11Fizz1314FizzBuzz16...
 -- 3: Buzz101Fizz103104FizzBuzz106...
 fizz_buzz_class_size :: Int -> Int
 fizz_buzz_class_size n = chp n + fb n where
  chp n = 9 * 10 ^ (n - 1) * (pre n + body n + suf n)
  fb n = pre_fb n + body_fb n + suf_fb n

  pre n = 
