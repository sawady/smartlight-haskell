module GameLoop where

import Screen
import Game
import Graphics.UI.SDL as SDL
import Common
import Control.Monad (liftM)
import Control.Exception (catch, IOException)

-- init
-- while(running) {    
--   gameLogic
--   render
-- }
-- cleanup

data GameLoop = GameLoop {
    _onInit       :: Game -> IO Game,
    _onGameLogic  :: Event -> Game -> IO Game,
    _onRender     :: Game -> IO (),
    _onCleanUp    :: Game -> IO ()
}

defaultGameLoop :: GameLoop
defaultGameLoop = GameLoop {
    _onInit       = defaultInit,
    _onGameLogic  = defaultLogic,
    _onRender     = defaultRender,
    _onCleanUp    = defaultCleanUp
}
    where
        defaultInit :: Game -> IO Game
        defaultInit = return
        
        defaultLogic :: Event -> Game -> IO Game
        defaultLogic ev game = return $
            case ev of
                Quit -> finish game
                _    -> game
                
        defaultRender :: Game -> IO ()
        defaultRender = SDL.flip . _screenSurface . _screen
                
        defaultCleanUp :: Game -> IO ()
        defaultCleanUp _ = return ()
       
newGameLoop :: GameLoop -> GameLoop
newGameLoop gl = GameLoop {
    _onInit       = extendsM _onInit defaultGameLoop gl,
    _onGameLogic  = \e -> extendsM (`_onGameLogic` e) defaultGameLoop gl,
    _onRender     = extendsM_ _onRender defaultGameLoop gl,
    _onCleanUp    = extendsM_ _onCleanUp defaultGameLoop gl
}
   
mainLoop :: GameLoop -> Game -> IO Game
mainLoop gl g = if _isRunning g then
  do newG <- events (_onGameLogic gl) g 
     _onRender gl newG
     SDL.delay 1000
     mainLoop gl newG
  else return g
  
  where
    events :: (Event -> Game -> IO Game) -> Game -> IO Game
    events gameLogic game = do
      event <- SDL.pollEvent
      case event of
        NoEvent -> return game
        _       -> gameLogic event game >>= events gameLogic
    
        
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