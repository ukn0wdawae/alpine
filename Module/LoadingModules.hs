import Data.List  
import Data.List (nub, sort) 
import Data.List hiding (nub) 
import qualified Data.Map
import qualified Data.Map as M 



-- load into ghci 
--ghci> :m + Data.List 
--ghci> :m + Data.List Data.Map Data.Set

--data.list
-- ghci> intersperse '.' "MONKEY"  
-- "M.O.N.K.E.Y"  
-- ghci> intersperse 0 [1,2,3,4,5,6]  
-- [1,0,2,0,3,0,4,0,5,0,6]

-- ghci> intercalate " " ["hey","there","folks"]  
-- "hey there folks"  
-- ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
-- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]  
-- [[1,4,7],[2,5,8],[3,6,9]]  
-- ghci> transpose ["hey","there","folks"]  
-- ["htf","eho","yel","rk","es"]

--ghci> concat ["foo","bar","car"]  
-- "foobarcar"  
-- ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
-- [3,4,5,2,3,4,2,1,1]  

--concatMap
-- ghci> concatMap (replicate 4) [1..3]  
-- [1,1,1,1,2,2,2,2,3,3,3,3]  

--any/and predicate
-- ghci> any (==4) [2,3,5,6,1,4]  
-- True  
-- ghci> all (>4) [6,9,10]  
-- True  
-- ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
-- False  
-- ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
-- True 

