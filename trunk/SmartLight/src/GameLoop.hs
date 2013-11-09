{-# LANGUAGE TemplateHaskell #-}
module GameLoop where

import Screen
import Game
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL
import Control.Lens.TH
import Control.Lens
import Control.Monad (liftM, when, (>=>))
import Control.Exception (catch, IOException)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Extras
import Common
import Data.Word

-- init
-- while(running) {    
--   gameLogic
--   render
-- }
-- cleanup

type GameState  a = Procedure (Game a)

data GameLoop a = GameLoop {
    _onInit       :: Game a -> IO (Game a),
    _byDefault    :: GameState a,
    _onEvent      :: GameState a,
    _onRender     :: Game a -> IO (),
    _onCleanUp    :: Game a -> IO ()
}

makeLenses ''GameLoop

defaultGameLoop :: GameLoop a
defaultGameLoop = GameLoop {
    _onInit       = defaultInit,
    _byDefault    = return (),
    _onEvent      = defaultLogic,
    _onRender     = defaultRender,
    _onCleanUp    = defaultCleanUp
}
    where
        defaultInit :: Game a -> IO (Game a)
        defaultInit game = do 
            _ <- SDL.enableKeyRepeat 10 40
            return game
        
        defaultLogic :: GameState a
        defaultLogic = do
                game <- get
                case view event game of
                      MouseMotion x y _ _ -> mousePos .= (fromIntegral x, fromIntegral y)
                      Quit -> modify finish
                      _    -> return ()
                 
        defaultRender :: Game a -> IO ()
        defaultRender g =
            SDL.flip . _screenSurface . _screen $ g
                
        defaultCleanUp :: Game a -> IO ()
        defaultCleanUp _ = return ()
        
addInit :: (Game a -> IO (Game a)) -> GameLoop a -> GameLoop a
addInit  f  = over onInit (>=> f)

addByDefaultLogic :: GameState a -> GameLoop a -> GameLoop a
addByDefaultLogic f = over byDefault (>> f)

addOnEventLogic :: GameState a -> GameLoop a -> GameLoop a
addOnEventLogic f = over onEvent (>> f)

addRender :: (Game a -> IO ()) -> GameLoop a -> GameLoop a
addRender f = over onRender (>> f)

addCleanUp :: (Game a -> IO ()) -> GameLoop a -> GameLoop a
addCleanUp f = over onCleanUp (>> f)

newGameLoop :: GameLoop a -> GameLoop a
newGameLoop gl = GameLoop {
    _onInit       = extendsM _onInit gl defaultGameLoop,
    _byDefault    = _byDefault gl >> _byDefault defaultGameLoop,
    _onEvent      = _onEvent gl >> _onEvent defaultGameLoop,
    _onRender     = extendsM_ _onRender gl defaultGameLoop,
    _onCleanUp    = extendsM_ _onCleanUp gl defaultGameLoop
}

simpleGameLoop :: GameState a ->  GameState a -> (Game a -> IO ()) -> GameLoop a
simpleGameLoop df l d = newGameLoop $ defaultGameLoop {
      _byDefault = df    
    , _onEvent   = l
    , _onRender  = d
}

loadingImages :: [String] -> GameLoop a -> GameLoop a
loadingImages xs = addInit (loadImages xs)

loadingFonts :: [(String,Int)] -> GameLoop a -> GameLoop a
loadingFonts xs = addInit (loadFonts xs)

controlFrameRate :: Word32 -> Word32 -> IO ()
controlFrameRate fps' ticks1 = do
    let allowed = div 1000 fps'
    when (ticks1 < allowed) $ do 
        ticks2 <- SDL.getTicks
        SDL.delay $ allowed - ticks2
            
mainLoop :: GameLoop a -> GameState a
mainLoop gl = 
    do
      g <- get
      when (_isRunning g) $ do 
          ticks <- lift SDL.getTicks
          events
          _byDefault gl
          g' <- get
          lift (_onRender gl g') 
          lift $ controlFrameRate (_fps g') ticks 
          mainLoop gl            

  where
    events = do
      ev <- lift SDL.pollEvent
      event .= ev
      _onEvent gl
      case ev of
        NoEvent -> return ()
        _       -> events
        
executeGame :: WindowData -> a -> GameLoop a -> IO ()
executeGame w gData gl = Control.Exception.catch execute abnormalQuit
    where
        execute :: IO ()
        execute = do
            SDL.init [SDL.InitEverything]
            _ <- TTF.init
            g  <- liftM (`newGame` gData) (createScreen w) >>= _onInit gl 
            g' <- execStateT (mainLoop gl) g
            _onCleanUp gl g'
            TTF.quit
            SDL.quit
        
        abnormalQuit :: IOException -> IO ()
        abnormalQuit e = do
            print e
            SDL.quit
            
executeSimpleGame :: (Int, Int) -> String -> a -> GameLoop a -> IO ()
executeSimpleGame screenSize t = executeGame (newWindowData screenSize t)            