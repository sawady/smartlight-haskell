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
      _pos = (20, snd midScreen)
    , _vel = (0, 30) 
}

newPlayer2 :: Player
newPlayer2 = (newEntity (PlayerData 0) "player") {
    _pos = (fst screenSize - 20, snd midScreen) 
}

moveUpPlayer :: Player -> Player
moveUpPlayer = velSub

moveDownPlayer :: Player -> Player
moveDownPlayer = velAdd