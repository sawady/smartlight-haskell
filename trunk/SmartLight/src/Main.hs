module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Control.Exception
import Control.Monad (when)

data Screen = Screen {
  _width   :: Int,
  _height  :: Int,
  _bpp     :: Int,
  _surface :: Surface
}

data Game = Game {
    _screen       :: Screen,
    _isRunning    :: Bool
}

data GameLoop = GameLoop {
    _onInit       :: IO Game,
    _onGameLogic  :: Game  -> Event -> IO Game,
    _onRender     :: Game  -> IO Game,
    _onCleanUp    :: Game  -> IO Game
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

events :: (Game -> Event -> IO Game) -> Game -> IO Game
events f game = do
  event <- SDL.pollEvent
  case event of
    NoEvent -> return game
    _       -> f game event >>= events f
    
onSurface :: Surface -> IO () -> IO ()
onSurface s c = do c; SDL.flip s

mainLoop :: GameLoop -> Game -> IO ()
mainLoop gl g =
    when (_isRunning g) $ do
        newG <- events (_onGameLogic gl) g >>= _onRender gl
        mainLoop gl newG
        
onExecute :: GameLoop -> IO ()
onExecute gl = do
    SDL.init [SDL.InitEverything]
    surface <- SDL.setVideoMode 640 480 32 [SWSurface]
    SDL.setCaption "" ""
    SDL.flip surface
    SDL.delay 5000
    SDL.quit
    
abnormalQuit :: IOException -> IO ()    
abnormalQuit e = do
    print e
    SDL.quit

main :: IO ()
main  = Control.Exception.catch (onExecute undefined) abnormalQuit