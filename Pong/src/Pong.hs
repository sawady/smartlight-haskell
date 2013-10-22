{-# LANGUAGE TemplateHaskell, RankNTypes, MultiWayIf #-}
module Pong where

import SmartLight
import Control.Lens.TH
import Control.Lens
import CommonPong
import Player
import Ball
import Control.Monad (when)

data Pong = Pong {
      _player1 :: Player
    , _player2 :: Player
    , _ball    :: Ball
}

makeLenses ''Pong

newPong :: Pong
newPong = Pong newPlayer1 newPlayer2 newBall

bounceBallOnEdges :: Procedure Pong
bounceBallOnEdges = do
    zoom ball $ do
        bounceOnEdgeX (fst screenSize)
        bounceOnEdgeY (snd screenSize)

onCollideWithPlayer :: Getter Pong Player -> Procedure Pong
onCollideWithPlayer playerLens = do
    g <- get
    zoom ball (bounceXOnCollide (view playerLens g))
                                
ballCollision :: Procedure Pong
ballCollision = do
    onCollideWithPlayer player1
    onCollideWithPlayer player2 
    
addPlayer1Point :: Procedure Pong
addPlayer1Point = addPlayerPoint player1 cond    
    where 
        cond g = view (ball . pos . _x) g >= fst screenSize

addPlayer2Point :: Procedure Pong
addPlayer2Point = addPlayerPoint player2 cond
    where 
        cond g = view (ball . pos . _x) g <= 0      

addPlayerPoint :: Setter' Pong Player -> (Pong -> Bool) -> Procedure Pong
addPlayerPoint pl p = do
    g <- get 
    when (p g) $ do
        pl.entityData.playerPoints += 1

pongByDefault :: GameState Pong
pongByDefault = do
    zoom gameData $ do
        bounceBallOnEdges
        zoom ball moveBall
        ballCollision 
        addPlayer1Point 
        addPlayer2Point

pongEventLogic :: GameState Pong
pongEventLogic = do
    g <- get
    zoom gameData $ 
      if | isKeyDown SDLK_DOWN g -> zoom player1 moveDownPlayer 
         | isKeyDown SDLK_UP g   -> zoom player1 moveUpPlayer
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