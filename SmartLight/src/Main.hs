module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Control.Exception
import Control.Monad (when)

data WindowData = WindowData {
  _width   :: Int,
  _height  :: Int,
  _bpp     :: Int,
  _title   :: String
}

data Screen = Screen {
  _screenData :: WindowData,
  _surface    :: Surface
}

data Game = Game {
    _screen       :: Screen,
    _isRunning    :: Bool
}

data GameLoop = GameLoop {
    _onInit       :: Game -> IO Game,
    _onGameLogic  :: Game -> Event -> Game,
    _onRender     :: Game -> IO (),
    _onCleanUp    :: Game -> IO ()
}

loadImage :: FilePath -> IO Surface
loadImage file = do 
    surfOld <- Image.load ("resources/" ++ file ++ ".bmp") 
    surfNew <- SDL.displayFormat surfOld
    SDL.freeSurface surfOld
    return surfNew

applySurface :: Int -> Int -> Surface -> Surface -> IO ()
applySurface x y source dest = do
    _ <- SDL.blitSurface source Nothing dest (Just rect)
    return ()
    where rect = Rect x y 0 0
    
type ResourceName = String

-- while(running) {    
-- events
-- loop
-- render
-- }

-- cleanup    

events :: (Game -> Event -> Game) -> Game -> IO Game
events gameLogic game = do
  event <- SDL.pollEvent
  case event of
    NoEvent -> return game
    _       -> events gameLogic (gameLogic game event)
    
onSurface :: Surface -> IO () -> IO ()
onSurface s c = do c; SDL.flip s

mainLoop :: GameLoop -> Game -> IO ()
mainLoop gl g = do
    g2 <- _onInit gl g
    when (_isRunning g2) $ do
        newG <- events (_onGameLogic gl) g2
        _onRender gl newG
        SDL.flip . _surface . _screen $ newG
        SDL.delay 1000
        mainLoop gl newG
        
createScreen :: WindowData -> IO Screen
createScreen w = do
     screenSurface <- SDL.setVideoMode (_width w) (_height w) (_bpp w) [SDL.HWSurface, SDL.DoubleBuf]
     SDL.setCaption (_title w) (_title w)
     return $ Screen w screenSurface
        
onExecute :: WindowData -> GameLoop -> IO ()
onExecute w gl = do
    SDL.init [SDL.InitEverything]
    screen <- createScreen w
    _onInit  gl (newGame screen) >>= mainLoop gl
    SDL.quit
    
executeGame :: WindowData -> GameLoop -> IO ()
executeGame w g = Control.Exception.catch (onExecute w g) abnormalQuit
    
finish :: Game -> Game
finish g = g { _isRunning = False }    
    
newGame :: Screen -> Game
newGame s = Game {
    _screen       = s,
    _isRunning    = True
}

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

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") newGameLoop