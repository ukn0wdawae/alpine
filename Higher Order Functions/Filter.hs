module Filter where

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  


-- takeWhile 
-- ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
-- 166650 

--same 
-- ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  
-- 166650 
--