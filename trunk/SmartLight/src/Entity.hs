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
      _entityName :: String
    , _pos        :: Pos
    , _vel        :: Vel
    , _entityData :: a
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

velAdd :: Entity a -> Entity a
velAdd e = over (pos.pY) (+ view (vel.vY) e) e

velSub :: Entity a -> Entity a
velSub e = over (pos.pY) (\x -> x - view (vel.vY) e) e