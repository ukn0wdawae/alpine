module Functor1 where
import Data.Char  
import Data.List 

-- Types that can act like a box can be functors.
class Functor f where  
    fmap :: (a -> b) -> f a -> f b 

-- map is just the list instance of functor
--Notice how we didn't write instance Functor [a] where, because from fmap :: (a -> b) -> f a -> f b, we see that the f has to be a type constructor that takes one type. [a] is already a concrete type (of a list with any type inside it), while [] is a type constructor that takes one type and can produce types such as [Int], [String] or even [[String]].
instance Functor [] where  
    fmap = map  

-- map :: (a -> b) -> [a] -> [b]  
-- ghci> fmap (*2) [1..3]  
-- [2,4,6]  
-- ghci> map (*2) [1..3]  
-- [2,4,6]  

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing

-- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
-- Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
-- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
-- Nothing  
-- ghci> fmap (*2) (Just 200)  
-- Just 400  
-- ghci> fmap (*2) Nothing  
-- Nothing  

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  

-- ghci> fmap (*2) EmptyTree  
-- EmptyTree  
-- ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
-- Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree



-- fmap for either :: (b -> c) -> Either a b -> Either a c
data Either a b = Left a | Right b
instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x  



instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
-- (\xs -> intersperse '-' (reverse (map toUpper xs))) with lambda
-- $ runhaskell fmapping_io.hs  
-- hello there  
-- E-R-E-H-T- -O-L-L-E-H  
    