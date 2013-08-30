{-# LANGUAGE TemplateHaskell #-}
module Entity where

import Control.Lens.TH
import Control.Lens

data Pos = Pos {
      _pX :: Int
    , _pY :: Int
}

data Vel = Vel {
      _vX :: Int
    , _vY :: Int
}

data Entity a = Entity {
      _entityName  :: String
    , _pos         :: Pos
    , _vel         :: Vel
    , _entityData  :: a
}

type PureEntity = Entity ()

makeLenses ''Pos
makeLenses ''Vel
makeLenses ''Entity

newPureEntity :: String -> PureEntity
newPureEntity = newEntity ()

newEntity :: a -> String -> Entity a
newEntity d n = Entity {
      _entityName = n
    , _pos        = Pos 0 0
    , _vel        = Vel 0 0
    , _entityData = d
}

velAddX :: Entity a -> Entity a
velAddX e = over (pos.pX) (+ view (vel.vX) e) e

velAddY :: Entity a -> Entity a
velAddY e = over (pos.pY) (+ view (vel.vY) e) e

velAdd :: Entity a -> Entity a
velAdd e = velAddX . velAddY $ e

velSubY :: Entity a -> Entity a
velSubY e = over (pos.pY) (\x -> x - view (vel.vY) e) e

velSubX :: Entity a -> Entity a
velSubX e = over (pos.pX) (\x -> x - view (vel.vX) e) e

velSub :: Entity a -> Entity a
velSub e = velSubX . velSubY $ e

bounceX :: Entity a -> Entity a
bounceX = (\b' -> over (pos . pX) (+ view (vel . vX) b' * 2) b') . over (vel  . vX) (* (-1))

bounceY :: Entity a -> Entity a
bounceY = (\b' -> over (pos . pY) (+ view (vel . vY) b' * 2) b') . over (vel  . vY) (* (-1))

bounceOnEdgeX :: Int -> Entity a ->Entity a
bounceOnEdgeX screenSizeX b = 
    if onEdgeX screenSizeX b 
       then bounceX b
       else b

bounceOnEdgeY :: Int -> Entity a -> Entity a
bounceOnEdgeY screenSizeY b = 
    if onEdgeY screenSizeY b  
       then bounceY b
       else b

onEdgeX :: Int -> Entity a ->Bool
onEdgeX screenSizeX e = x < (- screenSizeX `div` 2) || x > (screenSizeX `div` 2)
    where x = view (pos . pX) e
               
onEdgeY :: Int -> Entity a -> Bool
onEdgeY screenSizeY e = y < (- screenSizeY `div` 2) || y > (screenSizeY `div` 2)
    where y = view (pos . pY) e