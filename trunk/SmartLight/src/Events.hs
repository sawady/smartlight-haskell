module Events where

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Data.Bits

keyDown :: SDLKey -> Event
keyDown k = KeyDown (Keysym k [] '0')

keyUp :: SDLKey -> Event
keyUp k = KeyUp (Keysym k [] '0')

mouseX :: Event -> Int
mouseX (MouseMotion x _ _ _) = bitSize x
mouseX _                     = error "not mouse"

mouseY :: Event -> Int
mouseY (MouseMotion _ y _ _) = bitSize y
mouseY _                     = error "not mouse"
