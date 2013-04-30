module GameLoop where

import Screen
import Game
import Graphics.UI.SDL as SDL
import Control.Exception
import Control.Monad (when, liftM)

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
    
newGameLoop :: GameLoop
newGameLoop = GameLoop {
    _onInit       = return,
    _onGameLogic  = defaultLogic,
    _onRender     = defaultRender,
    _onCleanUp    = const (return ())
}

defaultLogic :: Game -> Event -> Game
defaultLogic game ev =
    case ev of
        Quit -> finish game
        _    -> game
        
defaultRender :: Game -> IO ()
defaultRender g = SDL.flip . _surface . _screen $ g
    

abnormalQuit :: IOException -> IO ()
abnormalQuit e = do
    print e
    SDL.quit
    
mainLoop :: GameLoop -> Game -> IO ()
mainLoop gl g = when (_isRunning g) $ do
  newG <- events (_onGameLogic gl) g
  _onRender gl newG
  SDL.delay 1000
  mainLoop gl newG
        
onExecute :: WindowData -> GameLoop -> IO ()
onExecute w gl = do
    SDL.init [SDL.InitEverything]
    liftM newGame (createScreen w) >>= _onInit gl  >>= mainLoop gl
    SDL.quit

executeGame :: WindowData -> GameLoop -> IO ()
executeGame w g = Control.Exception.catch (onExecute w g) abnormalQuit