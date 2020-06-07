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
    putStr "Coordinates:\n> "
    crdsRaw <- getLine
    putStrLn $ showBoard board