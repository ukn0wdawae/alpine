module HigherOrderism where

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

--Partial functions
-- ghci> let listOfFuncs = map (*) [0..]  
-- ghci> (listOfFuncs !! 4) 5  
-- 20  


-- ghci> multThree 3 4  
-- <interactive>:1:0:  
--     No instance for (Show (t -> t))  
--       arising from a use of `print' at <interactive>:1:0-12  
--     Possible fix: add an instance declaration for (Show (t -> t))  
--     In the expression: print it  
--     In a 'do' expression: print it 



($) :: (a -> b) -> a -> b  
f $ x = f x  

-- map ($ 3) [(4+), (10*), (^2), sqrt]  
-- [7.0,30.0,9.0,1.7320508075688772]


{-
ghci> applyTwice (+3) 10  
16  
ghci> applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
ghci> applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
ghci> applyTwice (multThree 2 2) 9  
144  
ghci> applyTwice (3:) [1]  
[3,3,1]  

-}

