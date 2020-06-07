module Board
    (   createBoard,
        showBoard,
        changeFieldValueInBoard,
        putShip,
        Board (..),
        Value (..),
        Field (..),
        Status (..)
    ) where
    
import Data.List       (sortBy, intercalate)
import Data.List.Split (chunksOf)

{-
self - explanatory
-}
data Value = Empty | Ship deriving(Read, Eq)

instance Show Value where
    show val = case val of
        Empty -> " "
        Ship -> "X"

{-
because we have fields which were checked
by other player and those, which were not
-}
data Status = Safe | Hit deriving(Show, Read, Eq)

data Field = Field { coords :: (Int, Int), 
            value :: Value,
            status :: Status
            } 
            deriving(Show, Read, Eq)
            
data Board = Board {fields :: [Field]}
            deriving(Show, Read, Eq)
            
createBoard :: Board
createBoard = Board fields
            where
                fields = map (\[a, b] -> Field (a,b) Empty Safe) $ combinations [[0..9], [0..9]]

--Moj Boze, wreszcie zrozumialam o co chodzi
-- nie wiem, jak, ale przynajmniej wiem co O.o
combinations :: [[a]] -> [[a]]
combinations []     = [[]]
combinations (xs:xss) =
    [ x:xs' | x <- xs, xs' <- combinations xss ]
  
showBoard :: Board -> String
showBoard board = concat $ map (\x -> intercalate " | " x ++ "\n") strings
    where
        --strings = chunksOf 10 (boardToString board)
        strings = map (map show) $ boardToSortedVals board
        
newCoords :: (Int, Int) -> Int -> Int -> [Field] -> [(Int, Int)]
--newCoords :: Int -> Int
-- down right
newCoords crds dir size fields
        | (dir == 1) = [coords x | x <- fields, (snd $ coords x) <= ((snd crds) + size), (fst $ coords x) == fst crds]
        | (dir == 2) = [coords x | x <- fields, (fst $ coords x) <= ((fst crds) + size), (snd $ coords x) == snd crds]
        | otherwise = []

putShip :: Board -> Field -> Int -> Int -> Board
putShip board@(Board fields) f@(Field crds v s) size dir = Board newFields
    where
        newC = newCoords crds dir size fields
        newFields = [Field newCrd Ship Safe | newCrd <- newC] ++ [x | x <- fields, not (elem (coords x) newC)]
        

changeValue :: Field -> Value -> Field
changeValue (Field crds val st) newVal = Field crds newVal st

changeFieldValueInBoard :: Board -> Field -> Value -> Board
changeFieldValueInBoard board@(Board fields) f@(Field crds v s) newVal = Board newFields 
    where 
        newFields = (changeValue f newVal) : [x | x <- fields, coords x /= crds]

-- czyli jednak trzeba bedzie wstawic funkcje sortujaca pola
-- a tak bardzo chcialam tego uniknac :(
-- ok, wiec chamsko ja ukradlam z poprzedniego projektu
boardToSortedVals :: Board -> [[Value]]
boardToSortedVals (Board fs) =
    map (map value) $
    map (sortBy (\x y -> compare (snd $ coords x) (snd $ coords y))) $
    chunksOf 10 $
    sortBy (\x y -> compare (fst $ coords x) (fst $ coords y)) fs
    
-- nie potrafie nawet wyrazic, jak bardzo nie lubie tego jezyka!


--board = createBoard
flds = [Field (0,0) Empty Safe, Field (0,1) Ship Hit]
board1 :: Board
board1 = Board flds
f :: Field
f = Field (0,1) Empty Hit


-- EXAMPLES
exBoard :: Board
exBoard = Board 
    [ Field (1,1) Empty Safe
    , Field (2,1) Empty Safe
    , Field (2,2) Ship Hit
    , Field (2,0) Empty Hit
    , Field (1,2) Ship Safe
    , Field (0,0) Ship Hit
    , Field (1,0) Empty Safe
    , Field (0,2) Ship Hit
    , Field (0,1) Ship Hit
    ]
