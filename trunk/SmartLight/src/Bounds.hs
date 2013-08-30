{-# LANGUAGE TemplateHaskell #-}
module Bounds where

import Entity
import Control.Lens.TH
import Control.Lens

data Bounds = RBound RectBound | CBound CircleBound

data RectBound = RectBound {
      _bPos :: Pos
    , _bW   :: Int
    , _bH   :: Int
}

data CircleBound = CircleBound {
      _cPos :: Pos
    , _cR   :: Int
}

makeLenses ''RectBound
makeLenses ''CircleBound

collideWithBounds :: Bounds -> Bounds -> Bool
collideWithBounds (RBound b1) (RBound b2) = collidesRectAgainstRect b1 b2
collideWithBounds _           _           = False

collidesRectAgainstRect :: RectBound -> RectBound -> Bool
collidesRectAgainstRect b1 b2 = 
    (x1 <= x2 && x2 < right1 || x2 <= x1 && x1 < right2) 
    && (y1 <= y2 && y2 < bottom1 || y2 <= y1 && y1 < bottom2)                    
        where right1 = x1 + d
              right2 = x2 + f
              bottom1 = y1 + e
              bottom2 = y2 + g
              x1 = view (bPos.pX) b1
              y1 = view (bPos.pY) b1
              d  = view bW b1 
              e  = view bH b1
              x2 = view (bPos.pX) b2
              y2 = view (bPos.pY) b2
              f  = view bW b2
              g  = view bH b2