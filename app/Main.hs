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

 -- 1: 12Fizz4BuzzFizz78Fizz
 -- 2: Buzz11Fizz1314FizzBuzz16...
 -- 3: Buzz101Fizz103104FizzBuzz106...
 fizz_buzz_class_size :: Int -> Int
 fizz_buzz_class_size 1 = 21
 fizz_buzz_class_size n = 6 * 10 ^ (n - 2) * fizz_buzz_period_size n

 fizz_buzz_period_size :: Int -> Int
 fizz_buzz_period_size n = 8 * n + 32

 -- 全体でn番目の文字がどのクラスに入っているか、そしてクラスの中で何番目か
 fizz_buzz_class :: Int -> (Int, Int)
 fizz_buzz_class n = go (n, 1) where
  go :: (Int, Int) -> (Int, Int)
  go n k = let c = fizz_buzz_class_size k in
   if n <= c then (n, k) else go (n - c, k + 1)
 
 -- クラスkの中でn番目の文字がどの周期に入っているか、その中で何番目か
 fizz_buzz_period :: (Int, Int) -> (Int, Int)
 fizz_buzz_period (n, k) = quotRem n (fizz_buzz_period_size k)

 -- ちょっと前に発言された整数は何か、どれだけ文字列を切り捨てないといけないか
 fizz_buzz_culc :: Int -> Int
 fizz_buzz_culc = undefined
