module Main where

 import Prelude

 main :: IO ()
 main = do
  s <- return 1000000000 :: IO Int
  putStrLn $ fizz_buzz_string_t s

 fizz_buzz_string_t :: Int -> String
 fizz_buzz_string_t s = take 20 $ fizz_buzz_string_d s $ fizz_buzz_estimate s

 fizz_buzz_string_d :: Int -> Int -> String
 fizz_buzz_string_d s c = let cut = fizz_buzz_length c in
  if cut <= s
   then drop (s - cut - 2) $ fizz_buzz_string c
   else error "The estimate failed!"

 fizz_buzz_string :: Int -> String
 fizz_buzz_string c = concat $ fizz_buzz_list c

 fizz_buzz_list :: Int -> [String]
 fizz_buzz_list c = map fizz_buzz [c..]

 fizz_buzz :: Int -> String
 fizz_buzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0                   = "Fizz"
  |                   n `mod` 5 == 0 = "Buzz"
  | otherwise                        = show n

 fizz_buzz_estimate :: Int -> Int
 fizz_buzz_estimate = undefined

 fizz_buzz_length :: Int -> Int
 fizz_buzz_length = undefined
