module Examples.Pong.Pong where

import SmartLight

data Ball = Ball {
    ballX :: Int,
    ballY :: Int
}

type Points = Int

data Player = Player {
    playerX :: Int,
    playerY :: Int,
    playerPoints :: Points
}

data PongData = PongData {
    player1 :: Player,
    player2 :: Player,
    ball :: Ball
}

newPong :: PongData
newPong = PongData {
    player1 = Player 0 0 0,
    player2 = Player 0 0 0,
    ball = Ball 0 0
}

type PongGame = Game PongData

screenSizeX, screenSizeY :: Int
screenSizeX = 640
screenSizeY = 480

pongLogic :: Event -> PongGame -> IO PongGame 
pongLogic e g | e == keyDown SDLK_DOWN = return g
              | e == keyDown SDLK_UP   = return g
              | otherwise = return g              

--pongLogic KeyUp   g =
--pongLogic (MouseMotion x y _ _) = 

pongRender :: PongGame -> IO ()
pongRender = undefined
    
pongLoop :: GameLoop PongData
pongLoop = simpleGameLoop ["ball", "player", "table"] pongLogic pongRender

main :: IO ()
main  = executeSimpleGame screenSizeX screenSizeY "Pong" newPong pongLoop