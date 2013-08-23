module Events where

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Control.Lens
import Game

isKeyDown, isKeyUp :: SDLKey -> Game a -> Bool
isKeyDown k g = isKeyDown' k (view event g)
isKeyUp   k g = isKeyUp'   k (view event g) 

isKeyDown', isKeyUp' :: SDLKey -> Event -> Bool

isKeyDown' k1 (KeyDown (Keysym k2 _ _)) = k1 == k2
isKeyDown' _ _ = False

isKeyUp'   k1 (KeyUp (Keysym k2 _ _))   = k1 == k2
isKeyUp' _ _ = False