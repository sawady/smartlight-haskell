module Text where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Common

loadFontWithPath :: FilePath -> FilePath -> Int -> IO Font
loadFontWithPath file srcFolder
  = openFont (srcFolder ++ "/" ++ file ++ ".ttf")

loadFontResource :: FilePath -> Int -> IO Font
loadFontResource file = loadFontWithPath file "./resources"

drawTextOnSurface :: Pos -> String -> Font -> Color -> Surface -> IO ()
drawTextOnSurface p text font c dest = do
       msg <- message
       _ <- SDL.blitSurface msg Nothing dest (Just rect)
       return ()
       
       where
         centerX = 0
         centerY = 0
         rect = Rect (x + 640 `div` 2 - centerX) (y + 480 `div` 2 - centerY) 0 0
         message = TTF.renderTextSolid font text c
         (x,y) = p