module CurriedFunctions where

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

-- same functions 
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x 
 
compareWithHundred2 :: (Num a, Ord a) => a -> Ordering  
compareWithHundred2 = compare 100  
--

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])



{-
ghci> max 4 5  
5  
ghci> (max 4) 5  
5

ghci> let multTwoWithNine = multThree 9  
ghci> multTwoWithNine 2 3  
54  
ghci> let multWithEighteen = multTwoWithNine 2  
ghci> multWithEighteen 10  
180  


ghci> multThree 3 4  
<interactive>:1:0:  
    No instance for (Show (t -> t))  
      arising from a use of `print' at <interactive>:1:0-12  
    Possible fix: add an instance declaration for (Show (t -> t))  
    In the expression: print it  
    In a 'do' expression: print it  

-}