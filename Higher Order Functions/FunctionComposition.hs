module FunctionComposition where

(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 

--same
-- ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
-- [-5,-3,-6,-7,-3,-2,-19,-24] 

-- ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
-- [-5,-3,-6,-7,-3,-2,-19,-24] 
--

--same
-- ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
-- [-14,-15,-27] 

-- ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
-- [-14,-15,-27] 

--


--same
fn x = ceiling (negate (tan (cos (max 50 x)))) 
--point free style
fn = ceiling . negate . tan . cos . max 50
--


--same 
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  

oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  


oddSquareSum :: Integer  
oddSquareSum = 
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  
    
--