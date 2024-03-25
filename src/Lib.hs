module Lib
  ( playGame,
    emptyBoard,
    Player (X, O),
    isWinner,
    placeMark,
  )
where

import Data.List (intercalate, intersperse, transpose)
import Data.Maybe (isJust, isNothing)

-- Define the player data type
data Player = X | O deriving (Eq, Show)

-- Define the board as a list of Maybe Player
type Board = [[Maybe Player]]

-- Create an empty board
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

-- Function to print the board
printBoard :: Board -> IO ()
printBoard = putStrLn . unlines . intersperse "-----------" . map (intercalate " | " . map showPlayer)

-- Function to show the player (X, O, or empty)
showPlayer :: Maybe Player -> String
showPlayer Nothing = "_"
showPlayer (Just X) = "X"
showPlayer (Just O) = "O"

-- Function to place a player's mark on the board
placeMark :: Board -> (Int, Int) -> Player -> Board
placeMark board (row, col) player =
  let (rowsBefore, currentRow : rowsAfter) = splitAt row board
      currentRow' = take col currentRow ++ [Just player] ++ drop (col + 1) currentRow
   in rowsBefore ++ [currentRow'] ++ rowsAfter

-- Function to check if a player has won
isWinner :: Player -> Board -> Bool
isWinner player board =
  any allEqual (rows ++ columns ++ diagonals)
  where
    rows = board
    columns = transpose board
    diagonals = [[board !! i !! i | i <- [0 .. 2]], [board !! i !! (2 - i) | i <- [0 .. 2]]]
    allEqual = all (== Just player)

-- Function to check if the board is full
isFull :: Board -> Bool
isFull = all (all isJust)

-- Function to check if the game is over
gameOver :: Board -> Maybe Player -> Bool
gameOver board player = isFull board || isNothing player

win :: Board -> Bool
win board = isWinner X board || isWinner O board

-- Function to alternate players
alternate :: Maybe Player -> Maybe Player
alternate (Just X) = Just O
alternate (Just O) = Just X
alternate Nothing = Nothing

-- Function to play the game
playGame :: Board -> Maybe Player -> IO ()
playGame board player = do
  printBoard board
  if gameOver board player
    then putStrLn "It's a draw!"
    else do
      if win board
        then putStrLn $ show (showPlayer (alternate player)) ++ " wins!"
        else do
          putStrLn $ "Player " ++ show player ++ ", enter your move (row column):"
          input <- getLine
          let [row, col] = map read (words input)
          if isValidMove board (row, col)
            then playGame (placeMark board (row - 1, col - 1) (fromJust player)) (alternate player)
            else do
              putStrLn "Invalid move, please try again."
              playGame board player

-- Function to check if a move is valid
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (row, col) =
  row >= 1 && row <= 3 && col >= 1 && col <= 3 && isEmptyCell board (row - 1, col - 1)

-- Function to check if a cell is empty
isEmptyCell :: Board -> (Int, Int) -> Bool
isEmptyCell board (row, col) = isNothing (board !! row !! col)

-- Helper function to extract a value from Maybe
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"
