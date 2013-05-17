module Main where

import SmartLight

--myGameLoop = defaultGameLoop {
--    _onInit   = \g -> 
--    _onRender = \g -> 
--}

data Point  = Point  { x :: Double, y :: Double }
    deriving (Show)
data Circle = Circle { center :: Point, radius :: Double }
    deriving (Show)

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") defaultGameLoop