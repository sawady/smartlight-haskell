module Image where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLImage
import Data.Maybe

data Image = Image {
    _surface        :: Surface,
    _partialSurface :: Maybe Rect
}

size :: Image -> (Int,Int)
size img = if isNothing (_partialSurface img)
              then (surfaceGetWidth s, surfaceGetHeight s) 
              else (rectW r, rectH r)
    where s = _surface img
          r = fromJust (_partialSurface img)

loadImage :: FilePath -> IO Surface
loadImage file = do 
    surfOld <- SDLImage.load ("resources/" ++ file ++ ".bmp") 
    surfNew <- SDL.displayFormat surfOld
    SDL.freeSurface surfOld
    return surfNew

newImage :: Surface -> Image
newImage s = Image {
    _surface        = s,
    _partialSurface = Nothing
}

newPartialImage :: Surface -> Rect -> Image
newPartialImage s r = (newImage s) {
     _partialSurface = Just r
}

drawImage :: Int -> Int -> Image -> Surface -> IO ()
drawImage x y source dest = do
       _ <- SDL.blitSurface (_surface source) (_partialSurface source) dest (Just rect)
       return ()
    where rect = Rect x y 0 0