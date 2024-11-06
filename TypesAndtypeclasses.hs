module TypesAndtypeclasses where

data Bool = False | True 
data Shape = Circle Float Float Float | Rectangle Float Float Float Float 

--value contructors
-- ghci> :t Circle  
-- Circle :: Float -> Float -> Float -> Shape  
-- ghci> :t Rectangle  
-- Rectangle :: Float -> Float -> Float -> Float -> Shape 


area :: Shape -> Float  
area (Circle _ _ r) = pi * r ^ 2  
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

-- ghci> area $ Circle 10 20 10  
-- 314.15927  
-- ghci> area $ Rectangle 0 0 100 100  
-- 10000.0  

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show) 

-- ghci> Circle 10 20 5  
-- Circle 10.0 20.0 5.0  
-- ghci> Rectangle 50 230 60 90  
-- Rectangle 50.0 230.0 60.0 90.0  

-- ghci> map (Circle 10 20) [4,5,6,6]  
-- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

data Point = Point Float Float deriving (Show)  
data Shape2 = Circle Point Float | Rectangle Point Point deriving (Show) 

area2 :: Shape2 -> Float  
area2 (Circle _ r) = pi * r ^ 2  
area2 (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
-- 10000.0  
-- ghci> surface (Circle (Point 0 0) 24)  
-- 1809.5574 

--move shape in x axis and y axis
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- ghci> nudge (Circle (Point 34 34) 10) 5 10  
-- Circle (Point 39.0 44.0) 10.0 

-- base circle at (0,0)
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
--base rect at (0,0)
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- ghci> nudge (baseRect 40 100) 60 23  
-- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)  


module Shapes  
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  


--record syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 


-- ghci> :t flavor  
-- flavor :: Person -> String  
-- ghci> :t firstName  
-- firstName :: Person -> String 

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

-- ghci> Car {company="Ford", model="Mustang", year=1967}  
-- Car {company = "Ford", model = "Mustang", year = 1967}  

--type constructor
data Maybe a = Nothing | Just a 

-- we call Maybe a type constructor.
-- if we pass Char as the type parameter to Maybe, we get a type of Maybe Char. The value Just 'a' has a type of Maybe Char.

-- ghci> Just "Haha"  
-- Just "Haha"  
-- ghci> Just 84  
-- Just 84  
-- ghci> :t Just "Haha"  
-- Just "Haha" :: Maybe [Char]  
-- ghci> :t Just 84  
-- Just 84 :: (Num t) => Maybe t  
-- ghci> :t Nothing  
-- Nothing :: Maybe a  
-- ghci> Just 10 :: Maybe Double  
-- Just 10.0 

-- allow multiple types
-- data Car = Car { company :: String  
--                , model :: String  
--                , year :: Int  
--                } deriving (Show) 

data Car2 a b c = Car2 { company :: a  
                     , model :: b  
                     , year :: c  
                     } deriving (Show)  

tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- ghci> let stang = Car {company="Ford", model="Mustang", year=1967}  
-- ghci> tellCar stang  
-- "This Ford Mustang was made in 1967" 

tellCar2 :: (Show a) => Car2 String String a -> String  
tellCar2 (Car2 {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


-- ghci> tellCar (Car "Ford" "Mustang" 1967)  
-- "This Ford Mustang was made in 1967"  
-- ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")  
-- "This Ford Mustang was made in \"nineteen sixty seven\""  
-- ghci> :t Car "Ford" "Mustang" 1967  
-- Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t  
-- ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"  
-- Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]  

-- typeclass constraint 
-- typeclass constraint in data declaration, data (Ord k) => Map k v = ...  
-- dont do this because you have to declare it in the functions anyway
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
-- Vector 12 7 16  
-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
-- Vector 12 9 19  
-- ghci> Vector 3 9 7 `vectMult` 10  
-- Vector 30 90 70  
-- ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
-- 74.0  
-- ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
-- Vector 148 666 222 


--derived instances
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq) 

-- ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
-- ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  
-- ghci> mca == adRock  
-- False  
-- ghci> mikeD == adRock  
-- False  
-- ghci> mikeD == mikeD  
-- True  
-- ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- True
-- ghci> let beastieBoys = [mca, adRock, mikeD]  
-- ghci> mikeD `elem` beastieBoys  
-- True

data Person2 = Person2{ firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read) 

-- ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- ghci> mikeD  
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- ghci> "mikeD is: " ++ show mikeD  
-- "mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

-- read (string to data)
-- ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD  
-- True  

--order is whatever is first 
data Bool = False | True deriving (Ord)

-- ghci> True `compare` False  
-- GT  
-- ghci> True > False  
-- True  
-- ghci> True < False  
-- False 

-- ghci> Nothing < Just 100  
-- True  
-- ghci> Nothing > Just (-49999)  
-- False  
-- ghci> Just 3 `compare` Just 2  
-- GT  
-- ghci> Just 100 > Just 50  
-- True 

--we can't do something like Just (*3) > Just (*2), because (*3) and (*2) are functions, which aren't instances of Ord.

--typeclasses
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

-- Show and Read
-- ghci> Wednesday  
-- Wednesday  
-- ghci> show Wednesday  
-- "Wednesday"  
-- ghci> read "Saturday" :: Day  
-- Saturday            

-- Eq and Ord
-- ghci> Saturday == Sunday  
-- False  
-- ghci> Saturday == Saturday  
-- True  
-- ghci> Saturday > Friday  
-- True  
-- ghci> Monday `compare` Wednesday  
-- LT 

-- Bounded
-- ghci> minBound :: Day  
-- Monday  
-- ghci> maxBound :: Day  
-- Sunday

-- Enum
-- ghci> succ Monday  
-- Tuesday  
-- ghci> pred Saturday  
-- Friday  
-- ghci> [Thursday .. Sunday]  
-- [Thursday,Friday,Saturday,Sunday]  
-- ghci> [minBound .. maxBound] :: [Day]  
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]


-- type synonyms
type String = [Char] 

phoneBook :: [(String,String)]  
phoneBook = 
    [("amelia","555-2938")  
    ,("freya","452-2928")  
    ,("isabella","493-2928")  
    ,("neil","205-2928")  
    ,("roald","939-8282")  
    ,("tenzing","853-2492")  
    ]  

type PhoneBook = [(String,String)]  

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)] 

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

--permits any k / v
type AssocList k v = [(k,v)] 

--curry with type
type IntMap = Map Int
-- type IntMap v = Map Int v


-- either
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  

-- ghci> Right 20  
-- Right 20  
-- ghci> Left "w00t"  
-- Left "w00t"  
-- ghci> :t Right 'a'  
-- Right 'a' :: Either a Char  
-- ghci> :t Left True  
-- Left True :: Either Bool b

-- to get a new locker with locker number (locker can be taken),  if taken dont give the code if not give the code
import qualified Data.Map as Map  
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  

-- key::Int    value::(LockerState, Code) 
type LockerMap = Map.Map Int (LockerState, Code) 


lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of  
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken  
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!" 

lockers :: LockerMap  
lockers = Map.fromList  
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

-- ghci> lockerLookup 101 lockers  
-- Right "JAH3I"  
-- ghci> lockerLookup 100 lockers  
-- Left "Locker 100 is already taken!"  
-- ghci> lockerLookup 102 lockers  
-- Left "Locker number 102 doesn't exist!"  
-- ghci> lockerLookup 110 lockers  
-- Left "Locker 110 is already taken!"  
-- ghci> lockerLookup 105 lockers  
-- Right "QOTSA"


data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord) 

infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

-- ghci> 3 :-: 4 :-: 5 :-: Empty  
-- (:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
-- ghci> 100 :-: a  
-- (:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty))) 

infixr 5  .++  
(.++) :: List a -> List a -> List a  
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  

-- we could implement all of the functions that operate on lists on our own list type.
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
-- ghci> let b = 6 :-: 7 :-: Empty  
-- ghci> a .++ b  
-- (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty)))) 


-- kind of types

-- ghci> :k Int  
-- Int :: * 
-- ghci> :k Maybe  
-- Maybe :: * -> * 
-- ghci> :k Maybe Int  
-- Maybe Int :: * 
-- ghci> :k Either  
-- Either :: * -> * -> *  
-- ghci> :k Either String  
-- Either String :: * -> *  
-- ghci> :k Either String Int  
-- Either String Int :: * 


-- assume concrete type * for a
-- j has to have a kind of * -> *
-- t has to have a kind of * -> (* -> *) -> *
class Tofu t where  
    tofu :: j a -> t a j  

-- data holds values so its end result is * 
-- assume * for a
-- b takes one type parameter and so its kind is * -> *
-- Frank has a kind of * -> (* -> *) -> *
data Frank a b  = Frank {frankField :: b a} deriving (Show)  

-- ghci> :t Frank {frankField = Just "HAHA"}  
-- Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe  
-- ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}  
-- Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree  
-- ghci> :t Frank {frankField = "YES"}  
-- Frank {frankField = "YES"} :: Frank Char []  

instance Tofu Frank where  
    tofu x = Frank x

-- ghci> tofu (Just 'a') :: Frank Char Maybe  
-- Frank {frankField = Just 'a'}  
-- ghci> tofu ["HELLO"] :: Frank [Char] []  
-- Frank {frankField = ["HELLO"]}

-- kind of Barry (* -> *) -> * -> * -> *
data Barry t k p = Barry { yabba :: p, dabba :: t k } 


--  to make this type a part of Functor we have to partially apply the first two type parameters so that we're left with 
-- * -> *. instance Functor (Barry a b) where
-- fmap as if it was made specifically for Barry, it would have a type of fmap :: (a -> b) -> Barry c d a -> Barry c d b
instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}  