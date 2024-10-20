module Foldl where

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc                -- Base case: empty list returns the accumulator
foldl f acc (x:xs) = foldl f (f acc x) xs  -- Apply the function f to the accumulator and head, then recurse

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False y

-- foldl (+) 0 [1, 2, 3, 4]
-- Steps:
-- foldl (+) (0 + 1) [2, 3, 4]
-- foldl (+) (1 + 2) [3, 4]
-- foldl (+) (3 + 3) [4]
-- foldl (+) (6 + 4) []
-- Result: 10