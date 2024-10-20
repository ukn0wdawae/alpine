module Map where

map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  

-- map function with foldr
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  


-- ghci> map (+3) [1,5,3,1,6]  
-- [4,8,6,4,9]  
-- ghci> map (++ "!") ["BIFF", "BANG", "POW"]  
-- ["BIFF!","BANG!","POW!"]  
-- ghci> map (replicate 3) [3..6]  
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
-- ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
-- [[1,4],[9,16,25,36],[49,64]]  
-- ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  
-- [1,3,6,2,2]