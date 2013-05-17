{-# LANGUAGE TemplateHaskell #-}
module Game where

import Screen
import Entity
import Data.Lens.Template

data Game = Game {
    _isRunning     :: Bool,
    _screen        :: Screen,
    _entityManager :: EntityManager
}

$( makeLenses [''Game] )

finish :: Game -> Game
finish g = g { _isRunning = False }

newGame :: Screen -> Game
newGame s = Game {
    _screen        = s,
    _isRunning     = True,
    _entityManager = newEntityManager
}