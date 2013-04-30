module Game where

import Screen

data Game = Game {
    _screen       :: Screen,
    _isRunning    :: Bool
}

finish :: Game -> Game
finish g = g { _isRunning = False }

newGame :: Screen -> Game
newGame s = Game {
    _screen       = s,
    _isRunning    = True
}