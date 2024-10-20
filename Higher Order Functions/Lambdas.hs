module Lambdas where

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 
    
-- with lambda
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100])) 

--Like normal functions, lambdas can take any number of parameters:
-- ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
-- [153.0,61.5,31.0,15.75,6.6]  
--ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  
-- [3,8,9,8,7] 


addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  

--same 

addThree :: (Num a) => a -> a -> a -> a  
addThree = \x -> \y -> \z -> x + y + z  