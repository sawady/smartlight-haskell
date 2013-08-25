module GameLoop where

import Screen
import Game
import Graphics.UI.SDL as SDL
import Common
import Control.Lens
import Control.Monad (liftM, when)
import Control.Exception (catch, IOException)

-- init
-- while(running) {    
--   gameLogic
--   render
-- }
-- cleanup

data GameLoop a = GameLoop {
    _onInit       :: Game a -> IO (Game a),
    _onGameLogic  :: Game a -> Game a,
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
        
        defaultLogic :: Game a -> Game a
        defaultLogic game =
                case view event game of
                    (MouseMotion x y _ _) -> set mousePos (fromIntegral x, fromIntegral y) game
                    Quit  -> finish game
                    _     -> game
                
        defaultRender :: Game a -> IO ()
        defaultRender g =
            SDL.flip . _screenSurface . _screen $ g
                
        defaultCleanUp :: Game a -> IO ()
        defaultCleanUp _ = return ()
       
newGameLoop :: GameLoop a -> GameLoop a
newGameLoop gl = GameLoop {
    _onInit       = extendsM _onInit defaultGameLoop gl,
    _onGameLogic  = extends _onGameLogic gl defaultGameLoop,
    _onRender     = extendsM_ _onRender gl defaultGameLoop,
    _onCleanUp    = extendsM_ _onCleanUp defaultGameLoop gl
}

simpleGameLoop :: [String] -> (Game a -> Game a) -> (Game a -> IO ()) -> GameLoop a
simpleGameLoop xs l d = newGameLoop $ defaultGameLoop {
      _onInit       = \g -> do
        newG <- loadEntities xs g
        _    <- SDL.enableKeyRepeat 10 40
        return newG
        
    , _onGameLogic  = l
    , _onRender     = d
}

controlFrameRate :: IO ()
controlFrameRate = do
     ticks1 <- SDL.getTicks
     when (ticks1 < 1000 `div` 10)
        (do
            ticks2 <- SDL.getTicks
            SDL.delay (( 1000 `div` 10 ) - ticks2))

mainLoop :: GameLoop a -> Game a -> IO (Game a)
mainLoop gl g = if _isRunning g then
  do 
     newG <- events g
     _onRender gl newG
     controlFrameRate
     mainLoop gl newG
  else return g
  
  where
--    events :: (Game a -> Game a) -> Game a -> IO (Game a)
    events game = do
      ev <- SDL.pollEvent
      let newG = _onGameLogic gl (set event ev game)
      case ev of
        NoEvent -> return newG
        _       -> events newG
        
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