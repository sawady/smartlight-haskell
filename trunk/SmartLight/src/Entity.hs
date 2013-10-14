{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Entity where

import Bounds
import Common
import Control.Lens.TH
import Control.Lens
import Extras
import Graphics.UI.SDL (Rect(Rect))

data Edge = TopEdge | BottomEdge | LeftEdge | RightEdge

data Entity a = Entity {
      _entityName   :: String
    , _pos          :: Pos
    , _vel          :: Vel
    , _bounds       :: Bounds
    , _entityData   :: a
}

makeLenses ''Entity

newVoidEntity :: String -> Entity ()
newVoidEntity = newEntity ()

newEntity :: a -> String -> Entity a
newEntity d n = Entity {
      _entityName = n
    , _pos        = (0,0)
    , _vel        = (0,0)
    , _bounds     = (0,0)
    , _entityData = d
}

velAddX :: Entity a -> Entity a
velAddX e = over (pos._x) (+ view (vel._x) e) e

velAddY :: Entity a -> Entity a
velAddY e = over (pos._y) (+ view (vel._y) e) e

velAdd :: Entity a -> Entity a
velAdd e = velAddX . velAddY $ e

velSubY :: Entity a -> Entity a
velSubY e = over (pos._y) (\x -> x - view (vel._y) e) e

velSubX :: Entity a -> Entity a
velSubX e = over (pos._x) (\x -> x - view (vel._x) e) e

velSub :: Entity a -> Entity a
velSub e = velSubX . velSubY $ e

changeDir :: Setter' Vel Int -> Entity a -> Entity a
changeDir axis = over (vel . axis) (* (-1))

bounceOnCollide :: Entity b -> Entity a -> Entity a
bounceOnCollide e2 e1 = on (collideWith e1 e2) (bounceY . bounceX) e1

bounceXOnCollide :: Entity b -> Entity a -> Entity a
bounceXOnCollide e2 e1 = on (collideWith e1 e2) bounceX e1

bounceYOnCollide :: Entity b -> Entity a -> Entity a
bounceYOnCollide e2 e1 = on (collideWith e1 e2) bounceY e1

bounceX :: Entity a -> Entity a
bounceX = velAddX . changeDir _x

bounceY :: Entity a -> Entity a
bounceY = velAddY . changeDir _y

bounceOnEdgeX :: Int -> Entity a -> Entity a
bounceOnEdgeX screenSizeX b = 
    if onEdgeX screenSizeX b 
       then bounceX b
       else b

bounceOnEdgeY :: Int -> Entity a -> Entity a
bounceOnEdgeY screenSizeY b = 
    if onEdgeY screenSizeY b  
       then bounceY b
       else b
       
onEdgeX :: Int -> Entity a -> Bool
onEdgeX screenSizeX e = x < 0 || x > screenSizeX
    where x = view (pos . _x) e
               
onEdgeY :: Int -> Entity a -> Bool
onEdgeY screenSizeY e = y < 0 || y > screenSizeY
    where y = view (pos . _y) e
    
boundPos :: Entity a -> Pos
boundPos e = boundsOrigin (view pos e) (view bounds e)
    where boundsOrigin (x,y) (w,h) = (x - div w 2, y - div h 2)

boundsRect :: Entity a -> Rect
boundsRect e = Rect x y (x+w) (y+h)
    where (x,y) = boundPos e
          (w,h) = view bounds e
    
collideWith :: Entity a -> Entity b -> Bool
collideWith e1 e2 = collideWithBounds (boundPos e1) b1 (boundPos e2) b2
    where b1 = view bounds e1
          b2 = view bounds e2
