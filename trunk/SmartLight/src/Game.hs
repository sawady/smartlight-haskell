{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Screen
import GameEntity
import Data.Lens.Template
import Data.Lens.Common
import qualified Data.HashMap.Strict as Map

type GameEntities   = Map.HashMap String GameEntity
type GameEntityName = String

data Game = Game {
    _isRunning  :: Bool,
    _screen     :: Screen,
    _entities :: GameEntities
}

$( makeLenses [''Game] )

addEntity :: GameEntityName -> GameEntity -> Game -> Game
addEntity n e = entities ^%= Map.insert n e 

removeEntity :: GameEntityName -> Game -> Game
removeEntity e = entities ^%= Map.delete e

newGame :: Screen -> Game
newGame s = Game {
    _screen     = s,
    _isRunning  = True,
    _entities   = Map.empty
}

finish :: Game -> Game
finish = isRunning ^= False