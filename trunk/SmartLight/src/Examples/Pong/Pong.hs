{-# LANGUAGE TemplateHaskell #-}
module Examples.Pong.Pong where

import SmartLight
import Control.Lens.TH
import Control.Lens

data Pos = Pos {
      _pX :: Int
    , _pY :: Int
}

data Vel = Vel {
      _vX :: Int
    , _vY :: Int
}

data Ball = Ball {
      _ballPos  :: Pos
    , _ballVel  :: Vel
}

type Points = Int

data Player = Player {
    _playerPos :: Pos,
    _playerPoints :: Points
}

data PongData = PongData {
    _player1 :: Player,
    _player2 :: Player,
    _ball :: Ball
}

makeLenses ''Pos
makeLenses ''Vel
makeLenses ''Ball
makeLenses ''Player
makeLenses ''PongData

newPong :: PongData
newPong = PongData {
    _player1 = Player (Pos (20 - screenSizeX `div` 2) 0) 0,
    _player2 = Player (Pos (screenSizeX `div` 2 - 20) 0) 0,
    _ball = Ball (Pos 0 0) (Vel 1 1)
}

type PongGame = Game PongData

playerVel :: Int
playerVel = 30

moveUpPlayer :: Player -> Player
moveUpPlayer = over (playerPos.pY) (\x -> x - playerVel) 

moveDownPlayer :: Player -> Player
moveDownPlayer = over (playerPos.pY) (+ playerVel)

moveBall :: Ball -> Ball
moveBall = moveBallX . moveBallY

moveBallX :: Ball -> Ball
moveBallX b = over (ballPos . pX) (\ x -> x + 1)
  (if onEdgeX (view (ballPos . pX) b) then bounceX b else b)


               
moveBallY :: Ball -> Ball
moveBallY b = over (ballPos . pY) (\ y -> y + 1)
  (if onEdgeY (view (ballPos . pY) b) then bounceY b else b)
               
bounceX :: Ball -> Ball
bounceX = over (ballVel  . vX) (\x -> x * (-1))           

bounceY :: Ball -> Ball
bounceY = over (ballVel  . vY) (\y -> y * (-1))   

onEdgeX :: Int -> Bool
onEdgeX x = x < 0 || x > screenSizeX
               
onEdgeY :: Int -> Bool
onEdgeY y = y < 0 || y > screenSizeY

screenSizeX, screenSizeY :: Int
screenSizeX = 640
screenSizeY = 480

pongLogic :: PongGame -> PongGame 
pongLogic = over (gameData.ball) moveBall . pongLogicInputs

pongLogicInputs :: PongGame -> PongGame 
pongLogicInputs g | isKeyDown SDLK_DOWN g = over (gameData.player1) moveDownPlayer g
                  | isKeyDown SDLK_UP g   = over (gameData.player1) moveUpPlayer g
                  | otherwise             = set  (gameData.player2.playerPos.pY) (mouseY g) g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    let d   = view gameData g
    let b   = view ball d
    let p1  = view player1 d
    let p2  = view player2 d
    
    drawEntity 0 0 "table" g
    drawEntity (view (playerPos.pX) p1) (view (playerPos.pY) p1) "player" g
    drawEntity (view (playerPos.pX) p2) (view (playerPos.pY) p2) "player" g
    drawEntity (view (ballPos.pX) b)    (view (ballPos.pY) b)    "ball"   g
    
    
pongLoop :: GameLoop PongData
pongLoop = simpleGameLoop ["ball", "player", "table"] pongLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop