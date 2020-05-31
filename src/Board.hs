module Board
    ( someFunc
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
        strings = chunksOf 10 (boardToString board)
       
  
boardToString :: Board -> [String]
boardToString (Board fields) = [show (value field) | field <- fields]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

changeValue :: Board -> Field -> Value -> Board
changeValue board field val = [(value f) = val | f <- (fields board), (coords f) == (coords field)]



--board = createBoard
flds = [Field (0,0) Empty Safe, Field (0,1) Ship Hit]


-- EXAMPLES
exBoard :: Board
exBoard = Board 
    [ Field (1,1) Empty Safe
    , Field (2,1) Empty Safe
    , Field (2,2) Ship Hit
{-    , Field (2,0) Empty Hit
    , Field (1,2) Ship Safe
    , Field (0,0) Ship Hit
    , Field (1,0) Empty Safe
    , Field (0,2) Ship Hit
    , Field (0,1) Ship Hit-}
    ]
