import System.Environment  
import System.IO  
import System.IO.Error
import System.Directory  
  

-- doesFileExist function from System.Directory
-- doesFileExist :: FilePath -> IO Bool
main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!" 


-- catch function from System.IO.Error
main = toTry `catch` handler1  
  
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler1 :: IOError -> IO ()  
handler1 e = putStrLn "Whoops, had some trouble!"

-- System.IO.Error — isDoesNotExistError and ioError
-- isDoesNotExistError is a predicate over IOErrors, which means that it's a function that takes an IOError and returns a True or False, meaning it has a type of isDoesNotExistError :: IOError -> Bool
-- ioError :: IOException -> IO a
handler2 :: IOError -> IO ()  
handler2 e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e 

-- notifyCops and freeSomeSpace are example I/O actions
-- These start with ioe and you can see a full list of them in the documentation
handler3 :: IOError -> IO ()  
handler3 e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | isFullError e = freeSomeSpace  
    | isIllegalOperation e = notifyCops  
    | otherwise = ioError e  

-- case expressions ioe functions
-- System.IO.Error also exports functions that enable us to ask our exceptions for some attributes, like what the handle of the file that caused the error is, or what the filename is. These start with ioe
-- ioeGetFileName :: IOError -> Maybe FilePath
handler4 :: IOError -> IO ()  
handler4 e  
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e  


-- toTry, thenTryThis and launchRockets are I/O actions that have been glued together using do syntax and hypothetically defined
main = do toTry `catch` handler1  
          thenTryThis `catch` handler2  
          launchRockets  