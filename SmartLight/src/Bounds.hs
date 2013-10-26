module Bounds where

import Common
import Control.Lens

-- (x,y) recivied here are the top-left corner of the entity 

type Bounds = (Int,Int)

_w :: Getter Bounds Int
_w = _1

_h :: Getter Bounds Int
_h = _2

right, bottom, left, top :: (Pos, Bounds) -> Int
left   ((x,_), (_,_)) = x
right  ((x,_), (w,_)) = x+w
top    ((_,y), (_,_)) = y
bottom ((_,y), (_,h)) = y+h

collideOnLeft :: (Pos, Bounds) -> (Pos, Bounds) -> Bool
collideOnLeft b1 b2 = left1 <= left2 && left2 < right1
        where right1   = right b1
              left1    = left b1
              left2    = left b2

collideOnRight :: (Pos, Bounds) -> (Pos, Bounds) -> Bool
collideOnRight b1 b2 = left2 <= left1 && left1 < right2
        where right2   = right b2
              left1    = left b1
              left2    = left b2
              
collideOnX :: (Pos, Bounds) -> (Pos, Bounds) -> Bool
collideOnX b1 b2 = collideOnLeft b1 b2 || collideOnRight b1 b2

collideOnY :: (Pos, Bounds) -> (Pos, Bounds) -> Bool
collideOnY b1 b2 = top1 <= top2 && top2 < bottom1 || top2 <= top1 && top1 < bottom2
        where bottom1 = bottom b1
              bottom2 = bottom b2
              top1    = top b1
              top2    = top b2

collide :: (Pos, Bounds) -> (Pos, Bounds) -> Bool
collide b1 b2 = collideOnX b1 b2 && collideOnY b1 b2
