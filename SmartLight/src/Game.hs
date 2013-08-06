{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Screen
import GameEntity
import Data.Lens.Template
import Data.Lens.Common
--import Data.List

type GameEntities = [GameEntity]

data Game = Game {
    _isRunning  :: Bool,
    _screen     :: Screen
--    _entities   :: GameEntities
}

$( makeLenses [''Game] )

--addEntity :: GameEntity -> Game -> Game
--addEntity e = entities ^%= (e:)
--
--removeEntity :: GameEntity -> Game -> Game
--removeEntity e = entities ^%= delete e

newGame :: Screen -> Game
newGame s = Game {
    _screen        = s,
    _isRunning     = True
--    _entities = []
}

finish :: Game -> Game
finish = isRunning ^= False