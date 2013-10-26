module Image where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLImage
import Common

data Image = Image {
    _surface        :: Surface,
    _center         :: (Int, Int),
    _rect           :: Rect
} deriving (Show)

imgOrigin :: Image -> (Int,Int)
imgOrigin img = (rectX r, rectY r)
    where r = _rect img

imgSize :: Image -> (Int,Int)
imgSize img = (rectW r, rectH r)
    where r = _rect img
          
loadImageFromPath :: FilePath -> FilePath -> IO Image          
loadImageFromPath file srcFolder = do
    img <- SDLImage.load (srcFolder ++ "/" ++ file ++ ".png")
    r   <- getClipRect img
    return Image {
          _surface = img
        , _center  = (rectW r `div` 2, rectH r `div` 2) 
        , _rect    = r
    }
    
loadImageResource :: FilePath -> IO Image
loadImageResource file = loadImageFromPath file "./resources"

newPartialImage :: Image -> Rect -> Image
newPartialImage img r = img {
         _rect    = r
       , _center  = (rectW r `div` 2, rectH r `div` 2)
}

drawImageOnSurface :: Pos -> Image -> Surface -> IO ()
drawImageOnSurface p source dest = do
       _ <- SDL.blitSurface (_surface source) (Just $ _rect source) dest (Just rect)
       return ()
        
       where
         centerX = fst $ _center source
         centerY = snd $ _center source
         rect    = Rect (x - centerX) (y - centerY) 0 0
         (x,y)   = p
