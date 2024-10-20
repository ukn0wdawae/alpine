module ZipWith where

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 


-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
-- [6,8,7,9]  
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]  
-- [7,3,2,5]  
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
-- ["foo fighters","bar hoppers","baz aldrin"]  
-- ghci> zipWith' (*) (replicate 5 2) [1..]  
-- [2,4,6,8,10]  
-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
-- [[3,4,6],[9,20,30],[10,12,12]] 