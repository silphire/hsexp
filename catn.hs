import System (getArgs)
import IO

catn :: String -> IO ()
catn fileName = bracket (openFile fileName ReadMode)
			hClose
			(\h -> do hSetBuffering h LineBuffering
				  printFileLine 1 h)
  where 
    printFileLine :: Int -> Handle -> IO ()
    printFileLine n hh = do eof <- hIsEOF hh
			    if eof then return ()
			           else do str <- hGetLine hh
					   putStrLn $ makeLine n str
					   printFileLine (n + 1) hh
    makeLine :: Int -> String -> String
    makeLine n str = (show n) ++ " " ++ str

main = do args <- getArgs
	  if (length args) == 0 then putStrLn "file not specified"
				else catn $ head args

