module Main where

import SmartLight

--myGameLoop = defaultGameLoop {
--    _onInit   = \g -> 
--    _onRender = \g -> 
--}

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") defaultGameLoop