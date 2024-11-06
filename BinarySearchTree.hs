module BinarySeachTree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)  
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 


-- building tree with fold
-- ghci> let nums = [8,6,4,1,7,3,5]  
-- ghci> let numsTree = foldr treeInsert EmptyTree nums  
-- ghci> numsTree  
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree)) 


-- typeclasses 

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y) 

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

-- ghci> Red == Red  
-- True  
-- ghci> Red == Yellow  
-- False  
-- ghci> Red `elem` [Red, Yellow, Green]  
-- True  
-- ghci> [Red, Yellow, Green]  
-- [Red light,Yellow light,Green light]

--subclasses
--That's all there is to subclassing, it's just a class constraint on a class declaration
class (Eq a) => Num a where  
   ...  

-- instances with non concrete types

-- not have m be a part of Eq typeclass makes Just x and Just y potentially not equatable
instance Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  

-- m is also part of the Eq typeclass
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False