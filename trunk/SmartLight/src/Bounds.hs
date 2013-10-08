module Bounds where

import Common
import Control.Lens

type Bounds = (Int,Int)

_w :: (Int -> Accessor Int Int) -> Bounds -> Accessor Int Bounds
_w = _1

_h :: (Int -> Accessor Int Int) -> Bounds -> Accessor Int Bounds
_h = _2

collideWithBounds :: Pos -> Bounds -> Pos -> Bounds -> Bool
collideWithBounds p1 b1 p2 b2 = 
    (x1 <= x2 && x2 < right1 || x2 <= x1 && x1 < right2) 
    && (y1 <= y2 && y2 < bottom1 || y2 <= y1 && y1 < bottom2)                    
        where right1 = x1 + d
              right2 = x2 + f
              bottom1 = y1 + e
              bottom2 = y2 + g
              x1 = view _x p1
              y1 = view _y p1
              d  = view _w b1 
              e  = view _h b1
              x2 = view _x p2
              y2 = view _y p2
              f  = view _w b2
              g  = view _h b2