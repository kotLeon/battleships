module Board
    (   createBoard,
        showBoard,
        changeFieldValueInBoard,
        putShip,
        readDirection,
        nc,
        someoneWon,
        shot,
        opponentBoard,
        Board (..),
        Value (..),
        Field (..)
    ) where
    
import Data.List       (sortBy, intercalate, find)
import Data.List.Split (chunksOf)

-- the board can have fields with values: empty (if there is no ship), ship(if there is a ship), hit (if there is a ship and it 
-- has been shot by opponent) and miss (if empty field has been shot)
data Value = Empty | Ship | Hit | Miss deriving(Read, Eq)

instance Show Value where
    show val = case val of
        Empty -> " "
        Ship -> "X"
        Hit -> "H"
        Miss -> "M"

data Field = Field { coords :: (Int, Int), 
                     value :: Value
                   } 
            deriving(Show, Read, Eq)
            
data Board = Board {fields :: [Field]}
            deriving(Show, Read, Eq)

data Direction = Rght | Down deriving(Show, Eq)

-- | readDirection takes Int and return Either Direction Strong; is used in putting ship on board
readDirection :: Int -> Either Direction String
readDirection i
  | i == 1 = Left Down
  | i == 2 = Left Rght
  | otherwise = Right "Invalid value read from direction"

-- | createBoard creates empty board
createBoard :: Board
createBoard = Board fields
            where
                fields = map (\[a, b] -> Field (a,b) Empty) $ combinations [[0..9], [0..9]]

combinations :: [[a]] -> [[a]]
combinations []     = [[]]
combinations (xs:xss) =
    [ x:xs' | x <- xs, xs' <- combinations xss ]
  
-- | showBoard is used in displaying board
showBoard :: Board -> String
showBoard board = concat $ map (\x -> intercalate " | " x ++ "\n") strings
    where
        strings = map (map show) $ boardToSortedVals board

-- | opponentBoard takes board and return board without ships
opponentBoard :: Board -> Board
opponentBoard board@(Board fields) = Board newFields 
    where 
        newFields = [if value x == Ship then changeValue x Empty else x | x <- fields]

-- | nc takes coordinates and finds next coordinate regarding what direction for the ship has been chosen
nc :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
nc crds dir size
  | (dir == Down) = [ (i, snd $ crds) | i <- [ fst crds .. fst crds + size - 1 ] ]
  | (dir == Rght) = [ (fst $ crds, j) | j <- [ snd crds .. snd crds + size - 1 ] ]

-- | putShip puts ship with given value on the board
putShip :: Board -> (Int, Int) -> Int -> Direction -> Board
putShip board@(Board fields) crds size dir = Board newFields
    where
        newC = nc crds dir size
        newFields = [Field newCrd Ship | newCrd <- newC] ++ [x | x <- fields, not (elem (coords x) newC)]
        
-- | shot change value on given coordinate regarding if a ship is there
shot :: Board -> Field -> Board
shot board@(Board fields) f@(Field crds v) = 
    changeFieldValueInBoard board f v
    where
        v = checkIfHit board crds

 --  checkIfHit checks if ship is on given cooordinate
checkIfHit :: Board -> (Int, Int) -> Value
checkIfHit board@(Board fields) crds =
    case r of
    Just x -> if (value x) == Ship || (value x) == Hit then Hit else  Miss
    Nothing -> error "Something went wrong"
    where
        r = find (\x -> coords x == crds) fields

changeValue :: Field -> Value -> Field
changeValue (Field crds val) newVal = Field crds newVal

changeFieldValueInBoard :: Board -> Field -> Value -> Board
changeFieldValueInBoard board@(Board fields) f@(Field crds v) newVal = Board newFields 
    where 
        newFields = (changeValue f newVal) : [x | x <- fields, coords x /= crds]


boardToSortedVals :: Board -> [[Value]]
boardToSortedVals (Board fs) =
    map (map value) $
    map (sortBy (\x y -> compare (snd $ coords x) (snd $ coords y))) $
    chunksOf 10 $
    sortBy (\x y -> compare (fst $ coords x) (fst $ coords y)) fs

-- | someoneWon chceck if there is any ship on the board
someoneWon :: Board -> Bool
someoneWon board@(Board fields) = 
    not $ any (\x -> value x == Ship) fields

-- --------------------- TESTS
--board = createBoard
flds = [Field (0,0) Empty, Field (0,1) Ship]
board1 :: Board
board1 = Board flds
f :: Field
f = Field (0,1) Empty


-- EXAMPLES
exBoard :: Board
exBoard = Board 
    [ Field (1,1) Hit
    , Field (2,1) Empty
    , Field (2,2) Ship
    , Field (2,0) Empty
    , Field (1,2) Miss
    , Field (0,0) Ship
    , Field (1,0) Empty
    , Field (0,2) Ship
    , Field (0,1) Ship
    ]

exBoard2 = Board 
    [ Field (1,1) Hit
    , Field (2,1) Empty
    , Field (2,2) Hit
    , Field (2,0) Empty
    , Field (1,2) Miss
    , Field (0,0) Hit
    , Field (1,0) Empty
    , Field (0,2) Hit
    , Field (0,1) Hit
    ]