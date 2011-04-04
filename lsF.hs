-- implementation of "ls -F ."
--   add suffix "*" if file is executable
--   add suffix "/" if file is a directory
--   add suffix "@" if file is a symbolic link

import System
import Directory
import System.Posix.Files

lsF :: String -> IO ()
lsF path = getDirectoryContents path >>= printFileName
  where
    printFileName :: [String] -> IO ()
    printFileName []      = return ()
    printFileName (f:fs)  = do str <- decorate $ relPath f
			       putStrLn str
			       printFileName fs
    
    decorate :: String -> IO String
    decorate fileName = 
      do p <- getPermissions fileName
	 st <- getSymbolicLinkStatus fileName
	 case (p, st) of
	   (p, st)  | (isSymbolicLink st) -> return (fileName ++ "@")
		    | (isDirectory st)	  -> return (fileName ++ "/")
		    | (executable p)	  -> return (fileName ++ "*")
		    | otherwise		  -> return fileName

    relPath :: String -> String
    relPath f
      | path == "."	   = f
      | (last path) == '/' = path ++ f
      | otherwise	   = path ++ "/" ++ f

main = do args <- getArgs
	  case args of
	    []	  -> lsF "."
	    args  -> lsF $ head args

