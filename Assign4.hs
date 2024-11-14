module Assign4 where
import Control.Monad.Cont
import Data.List 
import System.IO


cp :: FilePath -> FilePath -> IO ()
cp sourceFile targetFile = do
  withFile sourceFile ReadMode $ \hSource -> do
    contents <- hGetContents hSource
    withFile targetFile WriteMode $ \hTarget -> do
      hPutStr hTarget contents


indexCPS :: Eq a => a -> [a] -> (Int -> r) -> r -> r
indexCPS _ [] _ notFound = notFound
indexCPS x (y:ys) k notFound
  | x == y    = k 0
  | otherwise = indexCPS x ys (\v -> k (v + 1)) notFound


index_a :: Eq a => a -> [a] -> Maybe Int
index_a x xs = indexCPS x xs Just Nothing


index_b :: Eq a => a -> [a] -> Cont (Maybe Int) Int
index_b target = go 0
  where
    
    go _ [] = abortWith Nothing                    
    go n (x:xs)
      | x == target = return n                     
      | otherwise   = go (n + 1) xs               

(<<<) :: Cont r a -> (a -> r) -> r
m <<< k = runCont m k


topIndex_b :: Eq a => a -> [a] -> Maybe Int
topIndex_b target list = runCont (index_b target list) Just


abortWith :: r -> Cont r a
abortWith value = cont (\_ -> value)


index_c :: Eq a => a -> [a] -> Maybe Int
index_c target list = fmap (+0) (findIndex (== target) list)



index_d :: Eq a => a -> [a] -> Maybe Int
index_d target list = pure fst <*> find (\(_, x) -> x == target) (zip [0..] list)

index_e :: Eq a => a -> [a] -> Maybe Int
index_e target list =
  findIndex (== target) list >>= \index -> return index

index_f :: Eq a => a -> [a] -> Maybe Int
index_f target list = do
  index <- findIndex (== target) list
  return index

meetAndGreet :: IO ()
meetAndGreet = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello " ++ name ++ "!")

average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)


readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = do
  putStr prompt
  input <- getLine
  if input == sentinel
    then return []
    else do
      let parsedValue = read input :: Double
      rest <- readDoubles prompt sentinel
      return (parsedValue : rest)

interface :: IO ()
interface = do
  putStrLn "Enter some numbers."
  putStrLn "When finished, type 'done'."
  numbers <- readDoubles "Enter a number: " "done"
  if null numbers
    then putStrLn "No numbers were entered."
    else do
      let avg = average numbers
          maxVal = maximum numbers
          minVal = minimum numbers
      putStrLn $ "The average is " ++ show avg
      putStrLn $ "The maximum is " ++ show maxVal
      putStrLn $ "The minimum is " ++ show minVal

main :: IO ()
main = do
    cp "source.txt" "target.txt"