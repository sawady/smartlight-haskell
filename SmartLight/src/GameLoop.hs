module GameLoop where

import Screen
import Game
import Graphics.UI.SDL as SDL
import Control.Monad (liftM, (>=>))
import Control.Exception (catch, IOException)

data GameLoop = GameLoop {
    _onInit       :: Game -> IO Game,
    _onGameLogic  :: Event -> Game -> IO Game,
    _onRender     :: Game -> IO Game,
    _onCleanUp    :: Game -> IO Game
}

-- init
-- while(running) {    
--   gameLogic
--   render
-- }
-- cleanup   

events :: (Event -> Game -> IO Game) -> Game -> IO Game
events gameLogic game = do
  event <- SDL.pollEvent
  case event of
    NoEvent -> return game
    _       -> gameLogic event game >>= events gameLogic
    
extends :: Monad m => (t -> c -> m c) -> t -> t -> c -> m c
extends method g h = method g >=> method h

defaultGameLoop :: GameLoop
defaultGameLoop = GameLoop {
    _onInit       = defaultInit,
    _onGameLogic  = defaultLogic,
    _onRender     = defaultRender,
    _onCleanUp    = defaultCleanUp
}

newGameLoop :: GameLoop -> GameLoop
newGameLoop gl = GameLoop {
    _onInit       = extends _onInit defaultGameLoop gl,
    _onGameLogic  = \e -> extends (`_onGameLogic` e) defaultGameLoop gl,
    _onRender     = extends _onRender defaultGameLoop gl,
    _onCleanUp    = extends _onCleanUp defaultGameLoop gl
}

defaultInit :: Game -> IO Game
defaultInit = return

defaultCleanUp :: Game -> IO Game
defaultCleanUp = return

defaultLogic :: Event -> Game -> IO Game
defaultLogic ev game = return $
    case ev of
        Quit -> finish game
        _    -> game
        
defaultRender :: Game -> IO Game
defaultRender g = do
    SDL.flip . _surface . _screen $ g
    return g
    
mainLoop :: GameLoop -> Game -> IO Game
mainLoop gl g = if _isRunning g then
  do newG <- events (_onGameLogic gl) g
     newG' <- _onRender gl newG
     SDL.delay 1000
     mainLoop gl newG'
  else return g
        
executeGame :: WindowData -> GameLoop -> IO ()
executeGame w gl = Control.Exception.catch execute abnormalQuit
    where
        execute :: IO ()
        execute = do
            SDL.init [SDL.InitEverything]
            _ <- liftM newGame (createScreen w) >>= _onInit gl  >>= mainLoop gl >>= _onCleanUp gl
            SDL.quit
        
        abnormalQuit :: IOException -> IO ()
        abnormalQuit e = do
            print e
            SDL.quit