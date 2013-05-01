module Game where

import Screen
import Entity
import qualified Data.IntMap as IntMap

data Game = Game {
    _isRunning    :: Bool,
    _screen       :: Screen,
    _entities     :: IntMap.IntMap GameEntity
}

finish :: Game -> Game
finish g = g { _isRunning = False }

addEntity :: EntityData -> Game -> Game
addEntity e g = g {
    _entities = IntMap.insert newKey (GameEntity newKey e) (_entities g)
}
    where
        newKey = if null entKeys then 1 else maximum entKeys
        entKeys = IntMap.keys (_entities g)

removeEntity :: GameEntity -> Game -> Game
removeEntity e g = g {
    _entities = IntMap.delete (_entityId e) (_entities g)
}

newGame :: Screen -> Game
newGame s = Game {
    _screen       = s,
    _isRunning    = True,
    _entities     = IntMap.empty
}