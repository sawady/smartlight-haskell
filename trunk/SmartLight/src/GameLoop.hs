module GameLoop where

import Screen
import Game
import Graphics.UI.SDL as SDL
import Control.Exception
import Control.Monad (when)

data GameLoop = GameLoop {
    _onInit       :: Game -> IO Game,
    _onGameLogic  :: Game -> Event -> Game,
    _onRender     :: Game -> IO (),
    _onCleanUp    :: Game -> IO ()
}

-- init
-- while(running) {    
--   gameLogic
--   render
-- }
-- cleanup   

events :: (Game -> Event -> Game) -> Game -> IO Game
events gameLogic game = do
  event <- SDL.pollEvent
  case event of
    NoEvent -> return game
    _       -> events gameLogic (gameLogic game event)
    
onExecute :: WindowData -> GameLoop -> IO ()
onExecute w gl = do
    SDL.init [SDL.InitEverything]
    screen <- createScreen w
    _onInit  gl (newGame screen) >>= mainLoop gl
    SDL.quit
    
newGameLoop :: GameLoop
newGameLoop = GameLoop {
    _onInit       = return,
    _onGameLogic  = defaultLogic,
    _onRender     = \_ -> return (),
    _onCleanUp    = \_ -> return ()
}

defaultLogic :: Game -> Event -> Game
defaultLogic game ev =
    case ev of
        Quit -> finish game
        _    -> game

abnormalQuit :: IOException -> IO ()
abnormalQuit e = do
    print e
    SDL.quit

mainLoop :: GameLoop -> Game -> IO ()
mainLoop gl g = do
    g2 <- _onInit gl g
    when (_isRunning g2) $ do
        newG <- events (_onGameLogic gl) g2
        _onRender gl newG
        SDL.flip . _surface . _screen $ newG
        SDL.delay 1000
        mainLoop gl newG

executeGame :: WindowData -> GameLoop -> IO ()
executeGame w g = Control.Exception.catch (onExecute w g) abnormalQuit