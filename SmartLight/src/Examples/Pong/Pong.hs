module Examples.Pong.Pong where

import SmartLight

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

newPong :: PongData
newPong = PongData {
    _player1 = Player (20 - screenSizeX `div` 2) 0 0,
    _player2 = Player (screenSizeX `div` 2 - 20) 0 0,
    _ball = Ball 0 0
}

type PongGame = Game PongData

--moveUpPlayer1 :: PongGame -> PongGame

screenSizeX, screenSizeY :: Int
screenSizeX = 640
screenSizeY = 480

pongLogic :: Event -> PongGame -> PongGame 
pongLogic e g | isKeyDown SDLK_DOWN e = g
              | isKeyDown SDLK_UP e   = g
              | otherwise             = g
              
pongRender :: PongGame -> IO ()
pongRender g = do
    let pongData = _gameData g
    let ball     = _ball pongData
    let player1  = _player1 pongData
    let player2  = _player2 pongData
    
    drawEntity 0 0 "table" g
    drawEntity (_playerX player1) (_playerY player1) "player" g
    drawEntity (_playerX player2) (_playerY player2) "player" g
    drawEntity (_ballX ball) (_ballY ball) "ball" g
    
    
pongLoop :: GameLoop PongData
pongLoop = simpleGameLoop ["ball", "player", "table"] pongLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop