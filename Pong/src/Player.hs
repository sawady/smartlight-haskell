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

newPlayer1 :: Player
newPlayer1 = (newEntity (PlayerData 0) "player") {
      _pos = (20 - screenSizeX `div` 2, 0)
    , _vel = (0, 30) 
}

newPlayer2 :: Player
newPlayer2 = (newEntity (PlayerData 0) "player") {
    _pos = (screenSizeX `div` 2 - 20, 0) 
}

moveUpPlayer :: Player -> Player
moveUpPlayer = velSub

moveDownPlayer :: Player -> Player
moveDownPlayer = velAdd