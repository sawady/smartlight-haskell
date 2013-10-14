{-# LANGUAGE TemplateHaskell, RankNTypes, MultiWayIf #-}
module Pong where

import SmartLight
import Control.Lens.TH
import Control.Lens
import CommonPong
import Player
import Ball

data Pong = Pong {
      _player1 :: Player
    , _player2 :: Player
    , _ball    :: Ball
}

makeLenses ''Pong

newPong :: Pong
newPong = Pong newPlayer1 newPlayer2 newBall

onCollideWithPlayer :: Getter Pong Player -> Pong -> Pong
onCollideWithPlayer p g = over ball (bounceXOnCollide pl) g
    where pl = view p g
                                
ballCollision :: Pong -> Pong
ballCollision = onCollideWithPlayer player1 . onCollideWithPlayer player2 

addPlayer1Point :: Pong -> Pong
addPlayer1Point g = addPlayerPoint player1 cond g
    where cond = view (ball . pos . _x) g >= fst screenSize

addPlayer2Point :: Pong -> Pong
addPlayer2Point g = addPlayerPoint player2 cond g 
    where cond = view (ball . pos . _x) g <= 0      

addPlayerPoint :: Setter' Pong Player -> Bool -> Pong -> Pong
addPlayerPoint pl b = over (pl.entityData.playerPoints) addPoints
    where addPoints x = if b
                           then x + 1
                           else x
                               
pongByDefault :: GameState Pong
pongByDefault = do
    zoom gameData $ do
        modify ballCollision
        ball %= moveBall 
        modify addPlayer1Point 
        modify addPlayer2Point

pongEventLogic :: GameState Pong
pongEventLogic = do
    g <- get
    zoom gameData $ 
      if | isKeyDown SDLK_DOWN g -> modify (over player1 moveDownPlayer) 
         | isKeyDown SDLK_UP g   -> player1 %= moveUpPlayer
         | otherwise             -> player2 . pos . _y .= view (mousePos . _y) g
                 
drawPongScore :: Int -> Int -> Getter Pong Player -> Game Pong -> IO ()
drawPongScore x y pl g = drawText x y (view (gameData.pl.entityData.playerPoints) g) "scoreFont" (Color 255 255 255) g     

pongRender :: Game Pong -> IO ()
pongRender g = do
    drawImage midX midY "table" g
    drawEntity player1 g
    drawEntity player2 g 
    drawEntity ball g    
    drawPongScore (-46) 100 player1 g 
    drawPongScore 15 100 player2 g
    
    where midX = fst midScreen
          midY = snd midScreen  
    
pongLoop :: GameLoop Pong
pongLoop = 
    loadingImages ["ball", "player", "table"] $
    loadingFonts  [("scoreFont", 64)] $
    simpleGameLoop pongByDefault pongEventLogic pongRender

executePong :: IO ()
executePong  = executeSimpleGame screenSize "Pong" newPong pongLoop