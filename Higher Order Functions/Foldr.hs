module Foldr where

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc                 -- Base case: empty list returns the accumulator
foldr f acc (x:xs) = f x (foldr f acc xs)  -- Apply the function f to the head and recursive result

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs


-- foldr (+) 0 [1, 2, 3, 4]
-- Steps:
-- foldr (+) 0 [1, 2, 3, 4]
-- 1 + (foldr (+) 0 [2, 3, 4])
-- 1 + (2 + (foldr (+) 0 [3, 4]))
-- 1 + (2 + (3 + (foldr (+) 0 [4])))
-- 1 + (2 + (3 + (4 + 0)))
-- Result: 10