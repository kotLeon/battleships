module Main where

import Board

main :: IO ()
main = do
    putStrLn "Battleships!"
    putStrLn "Size of the board is 10x10"
    let board = createBoard
    let board2 = createBoard
    putStrLn $ showBoard board

    putStrLn "Player1"
    -- First ship
    putStr "Put your 2 field ship\n"
    putStr "Coordinates: (X,Y)\n"
    crdsRaw11 <- getLine
    putStr "Direction?(1-down or 2-right):\n"
    dirRaw11 <- getLine
    -- -- Second ship
    -- putStr "Put your 3 field ship\n"
    -- putStr "Coordinates: (X,Y)\n"
    -- crdsRaw12 <- getLine
    -- putStr "Direction?(1-down or 2-right):\n"
    -- dirRaw12 <- getLine

    putStrLn "Player2"
    -- First ship
    putStr "Put your 2 field ship\n"
    putStr "Coordinates: (X,Y)\n"
    crdsRaw21 <- getLine
    putStr "Direction?(1-down or 2-right):\n"
    dirRaw21 <- getLine

    -- -- Second ship
    -- putStr "Put your 2 field ship\n"
    -- putStr "Coordinates: (X,Y)\n"
    -- crdsRaw22 <- getLine
    -- putStr "Direction?(1-down or 2-right):\n"
    -- dirRaw22 <- getLine
    let crds11 = read crdsRaw11 :: (Int, Int)
        dir11 = readDirection (read dirRaw11::Int)
        board11 = putShip board crds11 2 dir11

        -- crds12 = read crdsRaw12 :: (Int, Int)
        -- dir12 = readDirection (read dirRaw12::Int)
        -- board12 = putShip board11 crds12 3 dir12

        crds21 = read crdsRaw21 :: (Int, Int)
        dir21 = readDirection (read dirRaw21::Int)
        board21 = putShip board2 crds21 2 dir21

        -- crds22 = read crdsRaw22 :: (Int, Int)
        -- dir22 = readDirection (read dirRaw11::Int)
        -- board22 = putShip board21 crds22 3 dir22

    move board11 board21
    
    
move :: Board -> Board -> IO ()
move board1 board2 = do
    if someoneWon board1 then putStrLn "Player2 won"
    else do 
        putStrLn "Player1\n"
        putStrLn "Your game"
        putStrLn $ showBoard board1
        putStrLn "Opponent"
        let opponent2 = opponentBoard board2
        putStrLn $ showBoard opponent2
        putStrLn "Shoot coordinates(X,Y)\n"
        newCoords <- getLine
        let crds = read newCoords :: (Int, Int)
            newBoard2 = shot board2 (Field crds Empty)
        if someoneWon newBoard2 then putStrLn "Player1 won"
        else do 
            putStrLn "Player2\n"
            putStrLn "Your game"
            putStrLn $ showBoard newBoard2
            putStrLn "Opponent"
            let opponent1 = opponentBoard board1
            putStrLn $ showBoard opponent1
            putStrLn "Shoot coordinates(X,Y)\n"
            newCoords2 <- getLine
            let crds2 = read newCoords2 :: (Int, Int)
                newBoard1 = shot board1 (Field crds2 Empty)
            move newBoard1 newBoard2


------------------------------------
