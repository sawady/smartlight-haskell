{-# LANGUAGE TemplateHaskell #-}
module Entity where

import Bounds
import Common
import Control.Lens.TH
import Control.Lens

data Entity a = Entity {
      _entityName  :: String
    , _pos         :: Pos
    , _vel         :: Vel
    , _bounds      :: Bounds
    , _entityData  :: a
}

type PureEntity = Entity ()

makeLenses ''Entity

newPureEntity :: String -> PureEntity
newPureEntity = newEntity ()

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

bounceX :: Entity a -> Entity b -> Entity a
bounceX = bounce _x _x

bounceY :: Entity a -> Entity b -> Entity a
bounceY = bounce _y _y

bounce axisSet axisGet e1 e2 = unMerge . changeDir $ e1
                 where changeDir = over (vel . axisSet) (* (-1))
                       unMerge newE1 = set (pos . axisSet) (view (pos . axisGet) newE1 - view (pos . axisGet) e2) newE1

bounceOnEdgeX :: Int -> Entity a ->Entity a
bounceOnEdgeX screenSizeX b = 
    if onEdgeX screenSizeX b 
       then bounceX b b
       else b

bounceOnEdgeY :: Int -> Entity a -> Entity a
bounceOnEdgeY screenSizeY b = 
    if onEdgeY screenSizeY b  
       then bounceY b b
       else b

onEdgeX :: Int -> Entity a -> Bool
onEdgeX screenSizeX e = x < (- midS) || x > midS
   
    where x = view (pos . _x) e
          midS = screenSizeX `div` 2
               
onEdgeY :: Int -> Entity a -> Bool
onEdgeY screenSizeY e = y < (- midS) || y > midS
    
    where y = view (pos . _y) e
          midS = screenSizeY `div` 2
    
collideWith :: Entity a -> Entity b -> Bool
collideWith e1 e2 = collideWithBounds (view pos e1) (view bounds e1) (view pos e2) (view bounds e2)