{-# LANGUAGE TemplateHaskell #-}
module Pong where

import SmartLight
import Control.Lens.TH
import Control.Lens
import CommonPong
import Player
import Ball

data PongData = PongData {
      _player1 :: Player
    , _player2 :: Player
    , _ball    :: Ball
}

makeLenses ''PongData

type PongGame = Game PongData

newPong :: PongData
newPong = PongData newPlayer1 newPlayer2 newBall

pongByDefault :: PongGame -> PongGame
pongByDefault = over (gameData.ball) moveBall

pongEventLogic :: PongGame -> PongGame 
pongEventLogic g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                 | isKeyDown SDLK_UP g  = over (gameData.player1) moveUpPlayer g
                 | otherwise            = set  (gameData.player2.pos.pY) (mouseY g) g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    drawImage 0 0 "table" g
    drawEntity player1 g
    drawEntity player2 g
    drawEntity ball    g
    
pongLoop :: GameLoop PongData
pongLoop = simpleGameLoop ["ball", "player", "table"] pongByDefault pongEventLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop