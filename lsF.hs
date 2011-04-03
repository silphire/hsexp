-- implementation of "ls -F ."
--   add suffix "*" if file is executable
--   add suffix "/" if file is a directory
--   add suffix "@" if file is a symbolic link

import Directory
import System.Posix.Files
import Monad

lsF :: String -> IO ()
lsF path = getDirectoryContents path >>= printFileName
  where
    printFileName :: [String] -> IO ()
    printFileName []      = return ()
    printFileName (f:fs)  = do str <- decorate f
			       putStrLn str
			       printFileName fs
    
    decorate :: String -> IO String
    decorate fileName = 
      do p <- getPermissions fileName
	 st <- getSymbolicLinkStatus fileName
	 if (isSymbolicLink st)
	    then return (fileName ++ "@")
	    else if (isDirectory st)
		    then return (fileName ++ "/")
		    else if (executable p)
			    then return (fileName ++ "*")
			    else return fileName

main = lsF "."

