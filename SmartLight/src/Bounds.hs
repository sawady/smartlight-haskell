module Bounds where

import Common
import Control.Lens

-- (x,y) recivied here are the top-left corner of the entity 

type Bounds = (Int,Int)

_w :: Getter Bounds Int
_w = _1

_h :: Getter Bounds Int
_h = _2

right, bottom, left, top :: Pos -> Bounds -> Int
left   (x,_) (_,_) = x
right  (x,_) (w,_) = x+w
top    (_,y) (_,_) = y
bottom (_,y) (_,h) = y+h

collideOnX :: Pos -> Bounds -> Pos -> Bounds -> Bool
collideOnX p1 b1 p2 b2 = left1 <= left2 && left2 < right1 || left2 <= left1 && left1 < right2
        where right1   = right p1 b1
              right2   = right p2 b2
              left1    = left p1 b1
              left2    = left p2 b2

collideOnY :: Pos -> Bounds -> Pos -> Bounds -> Bool
collideOnY p1 b1 p2 b2 = top1 <= top2 && top2 < bottom1 || top2 <= top1 && top1 < bottom2
        where bottom1 = bottom p1 b1
              bottom2 = bottom p2 b2
              top1    = top p1 b1
              top2    = top p2 b2

collide :: Pos -> Bounds -> Pos -> Bounds -> Bool
collide p1 b1 p2 b2 = collideOnX p1 b1 p2 b2 && collideOnY p1 b1 p2 b2
