module Main where

import Board

main :: IO ()
main = do
    putStrLn "Battleships!"
    putStrLn "Size of the board is 10x10"
    let board = createBoard
    putStrLn $ showBoard board
    putStr "Size of ship?\n> "
    sizeRaw <- getLine
    putStr "Coordinates:\n "
    crdsRaw <- getLine
    putStr "Direction?:\n "
    dirRaw <- getLine
    let newCrds = read crdsRaw :: (Int, Int)
        dir = read dirRaw :: Int
        size = read sizeRaw :: Int
        oldField = Field newCrds Empty Safe
        newBoard = putShip board oldField  size dir
    putStrLn $ showBoard newBoard