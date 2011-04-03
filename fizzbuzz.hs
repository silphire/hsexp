import System
import Data.List

fizzbuzzCall :: Int -> String
fizzbuzzCall n 
  | n `mod` 15 == 0	= "Fizz Buzz"
  | n `mod`  3 == 0	= "Fizz"
  | n `mod`  5 == 0	= "Buzz"
  | otherwise		= show n

fizzbuzz :: Int -> String
fizzbuzz n = unwords $ map fizzbuzzCall [1..n]

main = do args <- getArgs
	  if (length args) == 0 then putStrLn "argument required"
				else putStrLn $ fizzbuzz $ (read (head args) :: Int)

