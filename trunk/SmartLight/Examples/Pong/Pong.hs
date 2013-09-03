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

onCollideWithPlayer :: PongGame ->  Player -> Ball -> Ball
onCollideWithPlayer g p b = if collideWith b p g
                               then bounceX b
                               else b
                                
onCollideWithPlayer1, onCollideWithPlayer2 :: PongGame -> Ball -> Ball
onCollideWithPlayer1 g = onCollideWithPlayer g (view (gameData . player1) g)
onCollideWithPlayer2 g = onCollideWithPlayer g (view (gameData . player2) g)

pongByDefault :: PongGame -> PongGame
pongByDefault g = over (gameData.ball) 
                       (onCollideWithPlayer1 g . onCollideWithPlayer2 g . moveBall) g

pongEventLogic :: PongGame -> PongGame 
pongEventLogic g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                 | isKeyDown SDLK_UP g   = over (gameData.player1) moveUpPlayer g
                 | otherwise             = set  (gameData.player2.pos.pY) (mouseY g) g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    drawImage 0 0 "table" g
    drawEntity player1 g
    drawEntity player2 g
    drawEntity ball    g
    drawText 100 100 "0" "scoreFont" (Color 10 10 10) g
    
pongLoop :: GameLoop PongData
pongLoop = 
    loadingImages ["ball", "player", "table"] $
    loadingFonts  [("scoreFont", 32)] $
    simpleGameLoop pongByDefault pongEventLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop