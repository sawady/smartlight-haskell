module Screen where

import Graphics.UI.SDL as SDL

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

createScreen :: WindowData -> IO Screen
createScreen w = do
     screenSurface <- SDL.setVideoMode (_width w) (_height w) (_bpp w) [SDL.HWSurface, SDL.DoubleBuf]
     SDL.setCaption (_title w) (_title w)
     return $ Screen w screenSurface