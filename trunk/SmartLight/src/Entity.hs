{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Entity where

import Bounds
import Common
import Control.Lens.TH
import Control.Lens
import Graphics.UI.SDL (Rect(Rect))
import Control.Monad.Trans.State
import Control.Monad(when)

data Entity a = Entity {
      _entityName   :: String
    , _pos          :: Pos -- the position is the center of the entity
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

velAddX :: Procedure (Entity a)
velAddX = do
    e <- get
    pos._x += view (vel._x) e

velAddY :: Procedure (Entity a)
velAddY = do
    e <- get
    pos._y += view (vel._y) e

velAdd :: Procedure (Entity a)
velAdd = do
    velAddX
    velAddY
    
velSubX :: Procedure (Entity a)
velSubX = do
    e <- get 
    pos._x -= view (vel._x) e    

velSubY :: Procedure (Entity a)
velSubY = do
    e <- get
    pos._y -= view (vel._y) e

velSub :: Procedure (Entity a)
velSub = do
    velSubX
    velSubY

changeDir :: Setter' Vel Int -> Procedure (Entity a)
changeDir axis = vel . axis *= (-1)

bounceOnCollide :: Entity b -> Procedure (Entity a)
bounceOnCollide e2 = do
    e1 <- get
    when (collideWith e1 e2) $ do
         bounceX
         bounceY
         

bounceXOnCollide :: Entity b -> Procedure (Entity a)
bounceXOnCollide e2 = do
    e1 <- get
    when (collideWith e1 e2) bounceX

bounceYOnCollide :: Entity b -> Procedure (Entity a)
bounceYOnCollide e2 = do
    e1 <- get
    when (collideWith e1 e2) bounceY

bounceX :: Procedure (Entity a)
bounceX = do
    changeDir _x
    velAddX

bounceY :: Procedure (Entity a)
bounceY = do
    changeDir _y
    velAddY

bounceOnEdgeX :: Int -> Procedure (Entity a)
bounceOnEdgeX screenSizeX = do
    e <- get
    when (onEdgeX screenSizeX e) 
         bounceX

bounceOnEdgeY :: Int -> Procedure (Entity a)
bounceOnEdgeY screenSizeY = do
    e <- get
    when (onEdgeY screenSizeY e) 
         bounceY
       
onEdgeX :: Int -> Entity a -> Bool
onEdgeX screenSizeX e = onLeftEdge e || onRightEdge screenSizeX e
    
onLeftEdge :: Entity a -> Bool
onLeftEdge e  = view (pos . _x) e < 0   

onRightEdge :: Int -> Entity a -> Bool
onRightEdge screenSizeX e = view (pos . _x) e > screenSizeX
               
onEdgeY :: Int -> Entity a -> Bool
onEdgeY screenSizeY e = y < 0 || y > screenSizeY
    where y = view (pos . _y) e
    
boundPos :: Entity a -> Pos
boundPos e = boundsOrigin (view pos e) (view bounds e)
    where boundsOrigin (x,y) (w,h) = (x - div w 2, y - div h 2) -- because the position is in the center of the entity

boundsRect :: Entity a -> Rect
boundsRect e = Rect x y (x+w) (y+h)
    where (x,y) = boundPos e
          (w,h) = view bounds e
    
collideWith :: Entity a -> Entity b -> Bool
collideWith e1 e2 = collide (boundPos e1) b1 (boundPos e2) b2
    where b1 = view bounds e1
          b2 = view bounds e2
