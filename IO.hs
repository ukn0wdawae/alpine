module IO where
import Data.Char
import Control.Monad




-- ghci> :t putStrLn  
-- putStrLn :: String -> IO ()  
-- ghci> :t putStrLn "hello, world"  
-- putStrLn "hello, world" :: IO ()  
--putStrLn takes a string and returns an I/O action that has a result type of () (i.e. the empty tuple, also known as "unit")

--main always has a type signature of main :: IO something, where something is some concrete type. By convention, we don't usually specify a type declaration for main
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!") 

-- ghci> :t getLine  
-- getLine :: IO String


--in a do block, the last action cannot be bound to a name like the first two were
main = do  
    foo <- putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!") 

--use <- when you want to bind results of I/O actions to names and you can use let bindings to bind pure expressions to names
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  

--a program that continuously reads a line and prints out the same line with the words reversed. The program's execution will stop when we input a blank line.
main = do  
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 

--return doesn't cause the I/O do block to end in execution or anything like that
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line 


--return is sort of the opposite to <-. While return takes a value and wraps it up in a box, <- takes a box (and performs it) and takes the value out of it, binding it to a name.
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b  


-- $ runhaskell putstr_test.hs  
-- Hey, I'm Andy!  
-- putStr :: String -> IO ()
main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"

-- $ runhaskell putchar_test.hs  
-- teh 
main = do   putChar 't'  
            putChar 'e'  
            putChar 'h'  

-- putStr is recursively defined with putChar
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs  


-- print (putStrLn . show)
main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3] 


-- $ runhaskell print_test.hs  
-- True  
-- 2  
-- "haha"  
-- 3.2  
-- [3,4,3]  



-- ghci uses print to display things in terminal
-- ghci> 3  
-- 3  
-- ghci> print 3  
-- 3  
-- ghci> map (++"!") ["hey","ho","woo"]  
-- ["hey!","ho!","woo!"]  
-- ghci> print (map (++"!") ["hey","ho","woo"])  
-- ["hey!","ho!","woo!"] 


--getChar :: IO Char
-- $ runhaskell getchar_test.hs  
-- hello sir  
-- hello 
main = do  
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return () 
--same 
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  


--sequence :: [IO a] -> IO [a]
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  

-- is same as 

main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  


-- ghci> sequence (map print [1,2,3,4,5])  
-- 1  
-- 2  
-- 3  
-- 4  
-- 5  
-- [(),(),(),(),()] 


-- mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later.
-- ghci> mapM print [1,2,3]  
-- 1  
-- 2  
-- 3  
-- [(),(),()]  
-- ghci> mapM_ print [1,2,3]  
-- 1  
-- 2  
-- 3  




-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l


-- forM (located in Control.Monad) is like mapM, only that it has its parameters switched around. The first parameter is the list and the second one is the function to map over that list, which is then sequenced.
-- $ runhaskell form_test.hs  
-- Which color do you associate with the number 1?  
-- white  
-- Which color do you associate with the number 2?  
-- blue  
-- Which color do you associate with the number 3?  
-- red  
-- Which color do you associate with the number 4?  
-- orange  
-- The colors that you associate with 1, 2, 3 and 4 are:  
-- white  
-- blue  
-- red  
-- orange
main = do  
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  

-- program that takes some input and prints out only those lines that are shorter than 10 characters
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input = 
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  

--same code with interact

main = interact shortLinesOnly  
  
shortLinesOnly :: String -> String  
shortLinesOnly input = 
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  

-- same code in one line 

main = interact $ unlines . filter ((<10) . length) . lines 



-- i'm short  
-- so am i  
-- i am a loooooooooong line!!!  
-- yeah i'm long so what hahahaha!!!!!!  
-- short line  
-- loooooooooooooooooooooooooooong  
-- short
-- $ ghc --make shortlinesonly  
-- [1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )  
-- Linking shortlinesonly ...  
-- $ cat shortlines.txt | ./shortlinesonly  
-- i'm short  
-- so am i  
-- short 

-- program that continuously reads a line and then tells us if the line is a palindrome or not
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs 

main = interact respondPalindromes 

-- IOMode type 
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  

import System.IO  

-- Io mode returns an I/O action that will open the specified file in the specified mode. If we bind that action to something we get a Handle. A value of type Handle represents where our file is. We'll use that handle so we know which file to read from. It would be stupid to read a file but not bind that read to a handle because we wouldn't be able to do anything with the file. So in our case, we bound the handle to handle.
-- hGetContents. It takes a Handle, so it knows which file to get the contents from and returns an IO String — an I/O action that holds as its result the contents of the file. This function is pretty much like getContents.The only difference is that getContents will automatically read from the standard input (that is from the terminal), whereas hGetContents takes a file handle which tells it which file to read from.
--With putStr contents we just print the contents out to the standard output and then we do hClose, which takes a handle and returns an I/O action that closes the file. You have to close the file yourself after opening it with openFile!
-- openFile :: FilePath -> IOMode -> IO Handle
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  

--withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- same code with withFile 
main = do  
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)  

-- withFile function 
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode  
    result <- f handle  
    hClose handle  
    return result 
  


-- writeFile :: FilePath -> String -> IO ()
-- readFile :: FilePath -> IO String
main = do  
    contents <- readFile "girlfriend.txt"  
    writeFile "girlfriendcaps.txt" (map toUpper contents)

-- $ runhaskell girlfriendtocaps.hs  
-- $ cat girlfriendcaps.txt  
-- HEY! HEY! YOU! YOU!  
-- I DON'T LIKE YOUR GIRLFRIEND!  
-- NO WAY! NO WAY!  
-- I THINK YOU NEED A NEW ONE!  

-- appendFile :: FilePath -> String -> IO ()
-- appendFile unlike writeFile doesnt truncate the file to 0 length before adding new lines
-- program that takes a line from the standard input and adds that to our to-do list.
-- We needed to add the "\n" to the end of each line because getLine doesn't give us a newline character at the end.
main = do  
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")

-- $ runhaskell appendtodo.hs  
-- Iron the dishes  
-- $ runhaskell appendtodo.hs  
-- Dust the dog  
-- $ runhaskell appendtodo.hs  
-- Take salad out of the oven  
-- $ cat todo.txt  
-- Iron the dishes  
-- Dust the dog  
-- Take salad out of the oven  

--hSetBuffering function. It takes a handle and a BufferMode and returns an I/O action that sets the buffering. BufferMode is a simple enumeration data type and the possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int). The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing, then the operating system determines the chunk size. NoBuffering means that it will be read one character at a time. NoBuffering usually sucks as a buffering mode because it has to access the disk so much.
-- same code with block buff
main = do  
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents) 

-- removing an item from the to-do list
import System.Directory  
import Data.List  
  
main = do  
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"  
    numberString <- getLine  
    let number = read numberString  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"

-- $ runhaskell deletetodo.hs  
-- These are your TO-DO items:  
-- 0 - Iron the dishes  
-- 1 - Dust the dog  
-- 2 - Take salad out of the oven  
-- Which one do you want to delete?  
-- 1  
  
-- $ cat todo.txt  
-- Iron the dishes  
-- Take salad out of the oven  
  
-- $ runhaskell deletetodo.hs  
-- These are your TO-DO items:  
-- 0 - Iron the dishes  
-- 1 - Take salad out of the oven  
-- Which one do you want to delete?  
-- 0  
  
-- $ cat todo.txt  
-- Take salad out of the oven 

-- print program name and arguments
-- system.environment functions 
-- getArgs :: IO [String]
-- getProgName :: IO String
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName 
-- $ ./arg-test first second w00t "multi word arg"  
-- The arguments are:  
-- first  
-- second  
-- w00t  
-- multi word arg  
-- The program name is:  
-- arg-test


-- todo list with command line argument 

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") 

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName 

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle
    let number = read numberString  
        todoTasks = lines contents  
        bumpTask = (todoTasks !! number)
        newTodoItems = (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines $ bumpTask ++ newTodoItems    
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName


-- $ ./todo view todo.txt  
-- 0 - Iron the dishes  
-- 1 - Dust the dog  
-- 2 - Take salad out of the oven  
  
-- $ ./todo add todo.txt "Pick up children from drycleaners"  
  
-- $ ./todo view todo.txt  
-- 0 - Iron the dishes  
-- 1 - Dust the dog  
-- 2 - Take salad out of the oven  
-- 3 - Pick up children from drycleaners  
  
-- $ ./todo remove todo.txt 2  
  
-- $ ./todo view todo.txt  
-- 0 - Iron the dishes  
-- 1 - Dust the dog  
-- 2 - Pick up children from drycleaners 

