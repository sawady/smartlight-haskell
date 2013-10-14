{-# LANGUAGE TemplateHaskell #-}
module Player where

import SmartLight
import Control.Lens.TH
import CommonPong

type Player = Entity PlayerData

data PlayerData = PlayerData {
    _playerPoints :: Int
}

makeLenses ''PlayerData

newPlayerData :: PlayerData
newPlayerData = PlayerData 0

newPlayer :: Player
newPlayer = (newEntity newPlayerData "player") {
      _vel = (0, 30)
    , _bounds = (16,75) 
}

newPlayer1 :: Player
newPlayer1 = newPlayer {
      _pos = (20, snd midScreen)
}

newPlayer2 :: Player
newPlayer2 = newPlayer {
    _pos = (fst screenSize - 20, snd midScreen) 
}

moveUpPlayer :: Player -> Player
moveUpPlayer = velSub

moveDownPlayer :: Player -> Player
moveDownPlayer = velAdd