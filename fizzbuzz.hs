import Data.List

fizzbuzzCall :: Int -> String
fizzbuzzCall n 
  | n `mod` 15 == 0	= "Fizz Buzz"
  | n `mod`  3 == 0	= "Fizz"
  | n `mod`  5 == 0	= "Buzz"
  | otherwise		= show n

fizzbuzz :: Int -> String
fizzbuzz n = intercalate " " $ map fizzbuzzCall [1..n]

main = putStrLn $ fizzbuzz 20

