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

data GameLoop a = GameLoop {
    _onInit       :: Game a -> IO (Game a),
    _onGameLogic  :: Event -> Game a -> IO (Game a),
    _onRender     :: Game a -> IO (),
    _onCleanUp    :: Game a -> IO ()
}

defaultGameLoop :: GameLoop a
defaultGameLoop = GameLoop {
    _onInit       = defaultInit,
    _onGameLogic  = defaultLogic,
    _onRender     = defaultRender,
    _onCleanUp    = defaultCleanUp
}
    where
        defaultInit :: Game a -> IO (Game a)
        defaultInit = return
        
        defaultLogic :: Event -> Game a -> IO (Game a)
        defaultLogic ev game =
            return $
                case ev of
                    Quit -> finish game
                    _    -> game
                
        defaultRender :: Game a -> IO ()
        defaultRender g =
            SDL.flip . _screenSurface . _screen $ g
                
        defaultCleanUp :: Game a -> IO ()
        defaultCleanUp _ = return ()
       
newGameLoop :: GameLoop a -> GameLoop a
newGameLoop gl = GameLoop {
    _onInit       = extendsM _onInit defaultGameLoop gl,
    _onGameLogic  = \e -> extendsM (`_onGameLogic` e) defaultGameLoop gl,
    _onRender     = extendsM_ _onRender gl defaultGameLoop,
    _onCleanUp    = extendsM_ _onCleanUp defaultGameLoop gl
}

simpleGameLoop :: [String] -> (Event -> Game a -> IO (Game a)) -> (Game a -> IO ()) -> GameLoop a
simpleGameLoop xs l d = newGameLoop $ defaultGameLoop {
      _onInit       = loadEntities xs
    , _onGameLogic  = l
    , _onRender     = d
}

mainLoop :: GameLoop a -> Game a -> IO (Game a)
mainLoop gl g = if _isRunning g then
  do newG <- events (_onGameLogic gl) g 
     _onRender gl newG
     SDL.delay 5
     mainLoop gl newG
  else return g
  
  where
    events :: (Event -> Game a -> IO (Game a)) -> Game a -> IO (Game a)
    events gameLogic game = do
      event <- SDL.pollEvent
      case event of
        NoEvent -> return game
        _       -> gameLogic event game >>= events gameLogic
        
executeGame :: WindowData -> a -> GameLoop a -> IO ()
executeGame w g gl = Control.Exception.catch execute abnormalQuit
    where
        execute :: IO ()
        execute = do
            SDL.init [SDL.InitEverything]
            _ <- liftM (`newGame` g) (createScreen w) >>= _onInit gl  >>= mainLoop gl >>= _onCleanUp gl
            SDL.quit
        
        abnormalQuit :: IOException -> IO ()
        abnormalQuit e = do
            print e
            SDL.quit
            
executeSimpleGame :: Int -> Int -> String -> a -> GameLoop a -> IO ()
executeSimpleGame w h t = executeGame (newWindowData w h t)            