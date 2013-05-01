module Image where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image

data Image = Image {
    _surface   :: Surface
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