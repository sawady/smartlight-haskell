{-# LANGUAGE TemplateHaskell, RankNTypes #-}
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

onCollideWithPlayer :: Getter PongData Player -> PongData -> PongData
onCollideWithPlayer p g = over ball (bounceXOnCollide pl) g
    where pl = view p g
                                
ballCollision :: PongData -> PongData
ballCollision = onCollideWithPlayer player1 . onCollideWithPlayer player2 

addPlayer1Point :: PongData -> PongData
addPlayer1Point g = addPlayerPoint player1 cond g
    where cond = view (ball . pos . _x) g >= fst screenSize

addPlayer2Point :: PongData -> PongData
addPlayer2Point g = addPlayerPoint player2 cond g 
    where cond = view (ball . pos . _x) g <= 0      

addPlayerPoint :: Setter' PongData Player -> Bool -> PongData -> PongData
addPlayerPoint pl b = over (pl.entityData.playerPoints) addPoints
    where addPoints = if b
                         then (+1)
                         else id
                               
pongByDefault :: PongGame -> PongGame
pongByDefault = over gameData $ ballCollision . over ball moveBall . addPlayer1Point . addPlayer2Point

pongEventLogic :: PongGame -> PongGame 
pongEventLogic g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                 | isKeyDown SDLK_UP g   = over (gameData.player1) moveUpPlayer g
                 | otherwise             = set  (gameData.player2.pos._y) (view (mousePos._y) g) g
                 
drawPongScore :: Int -> Int -> Getter PongData Player -> PongGame -> IO ()
drawPongScore x y pl g = drawText x y (show $ view (gameData.pl.entityData.playerPoints) g) "scoreFont" (Color 255 255 255) g                 
              
pongRender :: PongGame -> IO ()
pongRender g = do
    drawImage midX midY "table" g
    drawEntity player1 g
    drawEntity player2 g 
    drawEntity ball g    
    drawPongScore (-46) 100 player1 g 
    drawPongScore 15 100 player2 g
    
    where midX = fst midScreen
          midY = snd midScreen  
    
pongLoop :: GameLoop PongData
pongLoop = 
    loadingImages ["ball", "player", "table"] $
    loadingFonts  [("scoreFont", 64)] $
    simpleGameLoop pongByDefault pongEventLogic pongRender

executePong :: IO ()
executePong  = executeSimpleGame screenSize "Pong" newPong pongLoop