{-# LANGUAGE TemplateHaskell #-}
module Examples.Pong.Pong where

import SmartLight
import Control.Lens.TH
import Control.Lens

data Ball = Ball {
    _ballX :: Int,
    _ballY :: Int
}

type Points = Int

data Player = Player {
    _playerX :: Int,
    _playerY :: Int,
    _playerPoints :: Points
} 

data PongData = PongData {
    _player1 :: Player,
    _player2 :: Player,
    _ball :: Ball
}

makeLenses ''Ball
makeLenses ''Player
makeLenses ''PongData

newPong :: PongData
newPong = PongData {
    _player1 = Player (20 - screenSizeX `div` 2) 0 0,
    _player2 = Player (screenSizeX `div` 2 - 20) 0 0,
    _ball = Ball 0 0
}

type PongGame = Game PongData

playerVel :: Int
playerVel = 30

ballSpeed :: Int
ballSpeed = 1

moveUpPlayer :: Player -> Player
moveUpPlayer = over playerY (\x -> x - playerVel) 

moveDownPlayer :: Player -> Player
moveDownPlayer = over playerY (+ playerVel)

moveBall :: Ball -> Ball
moveBall = moveBallX . moveBallY

moveBallX :: Ball -> Ball
moveBallX = over ballX (\x -> (x + ballSpeed) * bounceOnEdgeX x)

moveBallY :: Ball -> Ball
moveBallY = over ballY (\y -> (y + ballSpeed) * bounceOnEdgeY y)

bounce :: Bool -> Int
bounce True  = -1
bounce False = 1

bounceOnEdgeX :: Int -> Int
bounceOnEdgeX = bounce . outOfBounds
    where outOfBounds x = x < 0 || x > screenSizeX
    
bounceOnEdgeY :: Int -> Int
bounceOnEdgeY = bounce . outOfBounds
    where outOfBounds y = y < 0 || y > screenSizeY

screenSizeX, screenSizeY :: Int
screenSizeX = 640
screenSizeY = 480

pongLogic :: PongGame -> PongGame 
pongLogic = over (gameData.ball) moveBall . pongLogicInputs

pongLogicInputs :: PongGame -> PongGame 
pongLogicInputs g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                  | isKeyDown SDLK_UP g   = over (gameData.player1) moveUpPlayer g
                  | otherwise             = set  (gameData.player2.playerY) (mouseY g) g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    let d   = view gameData g
    let b   = view ball d
    let p1  = view player1 d
    let p2  = view player2 d
    
    drawEntity 0 0 "table" g
    drawEntity (view playerX p1) (view playerY p1) "player" g
    drawEntity (view playerX p2) (view playerY p2) "player" g
    drawEntity (view ballX b)    (view ballY b)    "ball"   g
    
    
pongLoop :: GameLoop PongData
pongLoop = simpleGameLoop ["ball", "player", "table"] pongLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop