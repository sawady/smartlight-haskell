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

onCollideWithPlayer p g = set (gameData.ball) 
    (if collideWith b pl
       then bounceX b pl
       else b) g
       
    where b  = view (gameData.ball) g
          pl = view (gameData.p) g       
                                
ballCollision :: PongGame -> PongGame
ballCollision = onCollideWithPlayer player1 . onCollideWithPlayer player2                                
                                
pongByDefault :: PongGame -> PongGame
pongByDefault = ballCollision . over (gameData.ball) moveBall

pongEventLogic :: PongGame -> PongGame 
pongEventLogic g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                 | isKeyDown SDLK_UP g   = over (gameData.player1) moveUpPlayer g
                 | otherwise             = set  (gameData.player2.pos._y) (mouseY g) g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    drawImage 0 0 "table" g
    drawEntity player1 g
    drawEntity player2 g
    drawEntity ball    g
    drawText (-46) 100 (show $ view (gameData.player1.entityData.playerPoints) g) "scoreFont" (Color 255 255 255) g
    drawText 15 100  (show $ view (gameData.player2.entityData.playerPoints) g) "scoreFont" (Color 255 255 255) g
    
pongLoop :: GameLoop PongData
pongLoop = 
    loadingImages ["ball", "player", "table"] $
    loadingFonts  [("scoreFont", 64)] $
    simpleGameLoop pongByDefault pongEventLogic pongRender

executePong :: IO ()
executePong  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop