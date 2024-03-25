module Main (main) where

import Lib (Player (X), emptyBoard, playGame)

-- Main function to start the game
main :: IO ()
main = playGame emptyBoard (Just X)
