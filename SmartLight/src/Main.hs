module Main where

import SmartLight

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") newGameLoop