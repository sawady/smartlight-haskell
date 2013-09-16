{-# LANGUAGE TemplateHaskell #-}
module Bounds where

import Common
import Control.Lens

data Bounds = Bounds {
      _bW   :: Int
    , _bH   :: Int
}

makeLenses ''Bounds

collideWithBounds :: Pos -> Bounds -> Pos -> Bounds -> Bool
collideWithBounds p1 b1 p2 b2 = 
    (x1 <= x2 && x2 < right1 || x2 <= x1 && x1 < right2) 
    && (y1 <= y2 && y2 < bottom1 || y2 <= y1 && y1 < bottom2)                    
        where right1 = x1 + d
              right2 = x2 + f
              bottom1 = y1 + e
              bottom2 = y2 + g
              x1 = view pX p1
              y1 = view pY p1
              d  = view bW b1 
              e  = view bH b1
              x2 = view pX p2
              y2 = view pY p2
              f  = view bW b2
              g  = view bH b2