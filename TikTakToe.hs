module TikTakToe where

import Control.Monad
import System.IO

-- 2D List for game board
type GameBoard = [[Square]]

data Square = Square Mark deriving (Show)
data Mark = Circle | Cross | Empty deriving (Show,Eq)
data Player = PlayerX | PlayerO | None deriving (Show,Eq)

-- Player as String
getPlayerName :: Player -> String
getPlayerName p = 
    if p==PlayerX
        then "PlayerX"
        else if p==PlayerO
            then "PlayerO"
            else "None"

-- Get Char marks for Squares
showSquare :: Square -> Char
showSquare (Square Circle) = 'O'
showSquare (Square Cross) = 'X'
showSquare (Square Empty) = ' '

-- Mark in Square
getMarkFromSquare :: Square -> Mark
getMarkFromSquare (Square mark) = mark

-- Check player for the mark
getPlayerForMark :: Mark -> Player
getPlayerForMark m = 
    if m==Cross
        then PlayerX
        else if m==Circle
            then PlayerO
            else None

-- Game State
data GameState = GameState {
    board :: GameBoard,
    turn :: Player,
    turnsLeft :: Int,
    inProgress :: Bool
} deriving (Show)

-- Accessing attributes of GameState

-- board
getGameStateBoard :: GameState -> GameBoard
getGameStateBoard (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = b

-- turn
getTurn :: GameState -> Player
getTurn (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = t

-- turnsLeft
getTurnsLeft :: GameState -> Int
getTurnsLeft (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = tl

-- inProgress
isGameInProgress :: GameState -> Bool
isGameInProgress (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = p

-- Functions in GameState

-- To update the next turn
nextTurn :: GameState -> Int -> GameState
nextTurn (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) pos =
    if (markAt ((pos-1) `div` 3) ((pos-1) `mod` 3) (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p})) /= Empty
        then -- If board at pos is not empty
            GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}
        else -- If board at pos is empty
            if t==PlayerX
            then -- PlayerO hit there 
                GameState {board=updateBoard b t pos, turn=PlayerO, turnsLeft=tl-1, inProgress=p}
            else -- PlayerX hit there
                GameState {board=updateBoard b t pos, turn=PlayerX, turnsLeft=tl-1, inProgress=p}

-- To end the game
endGame :: GameState -> GameState
endGame (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = GameState {board=b, turn=t, turnsLeft=tl, inProgress=False}

-- To get Mark at given row and column of board
markAt :: Int -> Int -> GameState -> Mark
markAt r c (GameState {board=b, turn=t, turnsLeft=tl, inProgress=p}) = getMarkFromSquare ((b!!r)!!c)

-- Update board with new turn played        
updateBoard :: GameBoard -> Player -> Int -> GameBoard
updateBoard gboard turn pos = 
    let
        hitSquare = if turn==PlayerX
                    then Square Cross
                    else Square Circle

        row = (pos-1) `div` 3
        col = (pos-1) `mod` 3

        (r1,_:r2) = splitAt row gboard
        (c1,_:c2) = splitAt col (gboard!!row)

        board = r1 ++ (c1++(hitSquare:c2)):r2

    in board


-- Checking who won
checkWhoWon :: GameState -> Player
checkWhoWon state = 
    let 
        -- Checking in rows
        winner1 = checkRowsForWinner state

        -- Checking in columns
        winner2 = if winner1 == None
                  then checkColumnsForWinner state
                  else winner1

        -- Checking in cross
        winner = if winner2 == None
                 then  checkCrossForWinner state
                 else winner2

    in winner

-- Checking for winner in rows
checkRowsForWinner :: GameState -> Player
checkRowsForWinner state = 
    let
        winner1 = None
    
        current1 = markAt 0 0 state
        winner2 = if (current1 /= Empty) && ((markAt 0 1 state)==current1) && ((markAt 0 2 state)==current1)
                then (getPlayerForMark current1)
                else winner1

        current2 = markAt 1 0 state
        winner3 = if (current2 /= Empty) && ((markAt 1 1 state)==current2) && ((markAt 1 2 state)==current2)
                then (getPlayerForMark current2)
                else winner2

        current3 = markAt 2 0 state
        winner = if (current3 /= Empty) && ((markAt 2 1 state)==current3) && ((markAt 2 2 state)==current3)
                then (getPlayerForMark current3)
                else winner3
    
    in winner

-- Checking for winner in columns
checkColumnsForWinner :: GameState -> Player
checkColumnsForWinner state = 
    let
        winner1 = None
    
        current1 = markAt 0 0 state
        winner2 = if (current1 /= Empty) && ((markAt 1 0 state)==current1) && ((markAt 2 0 state)==current1)
                then (getPlayerForMark current1)
                else winner1

        current2 = markAt 0 1 state
        winner3 = if (current2 /= Empty) && ((markAt 1 1 state)==current2) && ((markAt 2 1 state)==current2)
                then (getPlayerForMark current2)
                else winner2

        current3 = markAt 0 2 state
        winner = if (current3 /= Empty) && ((markAt 1 2 state)==current3) && ((markAt 2 2 state)==current3)
                then (getPlayerForMark current3)
                else winner3
    
    in winner

-- Checking for winner in cross
checkCrossForWinner :: GameState -> Player
checkCrossForWinner state = 
    let
        winner1 = None
    
        current1 = markAt 0 0 state
        winner2 = if (current1 /= Empty) && ((markAt 1 1 state)==current1) && ((markAt 2 2 state)==current1)
                then (getPlayerForMark current1)
                else winner1

        current2 = markAt 0 2 state
        winner = if (current2 /= Empty) && ((markAt 1 1 state)==current2) && ((markAt 2 0 state)==current2)
                then (getPlayerForMark current2)
                else winner2
    
    in winner
            
-- Game Loop
runGameLoop :: GameState -> IO ()
runGameLoop state = do

    putStr (getPlayerName (getTurn state))
    putStr (" Turn: ")
    hFlush stdout

    h <- getLine
    putStr "\ESC[2J"
    let hi = if (isGameInProgress state)
        then h
        else "-1"

    let hit = read hi :: Int

    let nextStatePrev = if hit>0
        then (nextTurn state hit)
        else (endGame state)

    let whoWon = checkWhoWon nextStatePrev

    let nextState = if (whoWon==None)
        then 
            if (getTurnsLeft nextStatePrev)<=0
                then (endGame nextStatePrev)
                else nextStatePrev
        else (endGame nextStatePrev)

    putStrLn ""
    drawBoard nextState

    if (isGameInProgress nextState)
        then
            runGameLoop nextState
        else
            if whoWon==None
                then putStrLn "Game Draw"
                else putStrLn (foldr (++) "" [getPlayerName whoWon," Won!"])

-- Drawing the board
drawBoard :: GameState -> IO ()
drawBoard state = do
    let boardStringList = getBoardString (getGameStateBoard state)
    putStrLn ("+---+---+---+")
    drawBoardRow (boardStringList!!0)
    putStrLn ("+---+---+---+")
    drawBoardRow (boardStringList!!1)
    putStrLn ("+---+---+---+")
    drawBoardRow (boardStringList!!2)
    putStrLn ("+---+---+---+")

-- Drawing Single row
drawBoardRow :: [Char] -> IO ()
drawBoardRow row = do
    putStr("| ")
    putStr([row!!0])
    putStr(" | ")
    putStr([row!!1])
    putStr(" | ")
    putStr([row!!2])
    putStrLn(" |")

-- Get board in string form (List of strings)
getBoardString :: GameBoard -> [[Char]]
getBoardString = map showRow
    where showRow = map showSquare

-- Initial game state
initialGameState :: GameState
initialGameState = GameState { 
    board = [
        [Square Empty,Square Empty,Square Empty],
        [Square Empty,Square Empty,Square Empty],
        [Square Empty,Square Empty,Square Empty]
    ],
    turn = PlayerX,
    turnsLeft = 9,
    inProgress = True
}

-- Starting the game
startGame :: IO ()
startGame = do
    
    putStrLn "+---+---+---+"
    putStrLn "| 1 | 2 | 3 |"
    putStrLn "+---+---+---+"
    putStrLn "| 4 | 5 | 6 |"
    putStrLn "+---+---+---+"
    putStrLn "| 7 | 8 | 9 |"
    putStrLn "+---+---+---+"

    runGameLoop initialGameState