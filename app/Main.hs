module Main where

 import Prelude

 main :: IO ()
 main = do
  s <- return 1 :: IO Int
  putStrLn $ fizz_buzz_string_r s

 fizz_buzz_string_r :: Int -> String
 fizz_buzz_string_r s =
  let
   (n, k) = fizz_buzz_class s
   (q, r) = fizz_buzz_period (n, k)
   (t, p) = fizz_buzz_culc k (q, r)
  in
   fizz_buzz_string_t t p

 fizz_buzz_string_t :: Int -> Int -> String
 fizz_buzz_string_t c s = take 20 $ fizz_buzz_string_d c s

 fizz_buzz_string_d :: Int -> Int -> String
 fizz_buzz_string_d c s = drop s $ fizz_buzz_string c

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
  go (n, k) = let c = fizz_buzz_class_size k in
   if n <= c then (n, k) else go (n - c, k + 1)

 -- クラスkの中でn番目の文字がどの周期に入っているか、その中で何番目か
 fizz_buzz_period :: (Int, Int) -> (Int, Int)
 fizz_buzz_period (n, k) = quotRem n (fizz_buzz_period_size k)

 -- ちょっと前に発言された整数は何か、どれだけ文字列を切り捨てないといけないか
 fizz_buzz_culc :: Int -> (Int, Int) -> (Int, Int)
 fizz_buzz_culc k (q, r) = ((10 ^ (k - 1) - 1) + q * 15 + 1, r)
