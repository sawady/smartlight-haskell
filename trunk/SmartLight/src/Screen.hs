{-# LANGUAGE TemplateHaskell #-}
module Screen where

import Graphics.UI.SDL as SDL
import Control.Lens.TH

data WindowData = WindowData {
  _width   :: Int,
  _height  :: Int,
  _bpp     :: Int,
  _title   :: String
}

data Screen = Screen {
  _screenData    :: WindowData,
  _screenSurface :: Surface
}

makeLenses ''WindowData
makeLenses ''Screen

newWindowData :: (Int, Int) -> String -> WindowData
newWindowData (w, h) t = WindowData {
    _width   = w
  , _height  = h
  , _bpp     = 32
  , _title   = t
}

createScreen :: WindowData -> IO Screen
createScreen w = do
     scrnsrfc <- SDL.setVideoMode (_width w) (_height w) (_bpp w) [SDL.HWSurface, SDL.DoubleBuf]
     SDL.setCaption (_title w) (_title w)
     return $ Screen w scrnsrfc