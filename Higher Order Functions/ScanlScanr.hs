module ScanlScanr where

-- ghci> scanl (+) 0 [3,5,2,1]  
-- [0,3,8,10,11]  
-- ghci> scanr (+) 0 [3,5,2,1]  
-- [11,8,3,1,0]  
-- ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
-- [3,4,5,5,7,9,9,9]  
-- ghci> scanl (flip (:)) [] [3,2,1]  
-- [[],[3],[2,3],[1,2,3]]

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  


-- ghci> sqrtSums  
-- 131  
-- ghci> sum (map sqrt [1..131])  
-- 1005.0942035344083  
-- ghci> sum (map sqrt [1..130])  
-- 993.6486803921487