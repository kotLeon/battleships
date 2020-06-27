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
    board11 <- makeShip board 2

    putStrLn "Player2"
    board21 <- makeShip board 2
   
    move board11 board21
    
makeShip :: Board -> Int -> IO Board
makeShip board size = do
    putStr $ "Put your " ++ (show size) ++ " field ship\n"
    putStr "Coordinates: (X,Y)\n"
    coordinatesRaw <- getLine
    putStr "Direction?(1-down or 2-right):\n"
    directionRaw <- getLine

    let coordinates = read coordinatesRaw :: (Int, Int)
        direction = readDirection (read directionRaw::Int)
    case direction of
        Left (dir) -> do
            let bb = putShip board coordinates size dir
            return bb
        Right (err) -> do
            putStr $ err ++ "\n"
            makeShip board size


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
