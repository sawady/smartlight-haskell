module Main where

import SmartLight

data Symbols = X | O

type Board = [[Maybe Symbols]]

data TicTacToe = TicTacToe

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") defaultGameLoop