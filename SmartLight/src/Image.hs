module Image where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLImage
import Data.Maybe

data Image = Image {
    _surface        :: Surface,
    _center         :: (Int, Int),
    _rect           :: Rect,
    _partialSurface :: Maybe Rect
} deriving (Show)

imgOrigin :: Image -> (Int,Int)
imgOrigin img = (rectX r, rectY r)
    where r = fromMaybe (_rect img) (_partialSurface img)

imgSize :: Image -> (Int,Int)
imgSize img = if isNothing (_partialSurface img)
              then (surfaceGetWidth s, surfaceGetHeight s) 
              else (rectW r, rectH r)
    where s = _surface img
          r = fromJust (_partialSurface img)
          
loadImageWithPath :: FilePath -> FilePath -> IO Surface          
loadImageWithPath file srcFolder =
    SDLImage.load (srcFolder ++ "/" ++ file ++ ".png")
    
-- old version
--    surfOld <- SDLImage.load (srcFolder ++ "/" ++ file ++ ".png")
--    surfNew <- SDL.displayFormat surfOld
--    SDL.freeSurface surfOld    

getArea :: Surface -> IO Rect
getArea = getClipRect
    
loadImage :: FilePath -> IO Surface
loadImage file = loadImageWithPath file "./resources"

newImage :: Surface -> Rect -> Image
newImage s r = Image {
      _surface        = s
    , _partialSurface = Nothing
    , _center         = (rectW r `div` 2, rectH r `div` 2) 
    , _rect           = r
}

newPartialImage :: Surface -> Rect -> Image
newPartialImage s r = (newImage s r) {
       _partialSurface = Just r
}

drawImageOnSurface :: Int -> Int -> Image -> Surface -> IO ()
drawImageOnSurface x y source dest = do
       _ <- SDL.blitSurface (_surface source) (_partialSurface source) dest (Just rect)
       return ()
       
       where
         centerX = fst $ _center source
         centerY = snd $ _center source
         rect = Rect (x + 640 `div` 2 - centerX) (y + 480 `div` 2 - centerY) 0 0
