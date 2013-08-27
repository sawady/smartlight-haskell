{-# LANGUAGE TemplateHaskell #-}
module Entity where

import Control.Lens.TH
import Control.Lens

type PureEntity = Entity ()

data Pos = Pos {
      _pX :: Int
    , _pY :: Int
}

data Vel = Vel {
      _vX :: Int
    , _vY :: Int
}

data Entity a = Entity {
--      _onInput    :: Entity a -> Entity a
--    , _always     :: Entity a -> Entity a
      _entityName :: String
    , _pos        :: Pos
    , _vel        :: Vel
    , _entityData :: a
}

makeLenses ''Pos
makeLenses ''Vel
makeLenses ''Entity

newPureEntity :: String -> PureEntity
newPureEntity = newEntity ()

newEntity :: a -> String -> Entity a
newEntity d n = Entity {
--      _onEvent    = id
--    , _default    = id
     _entityName = n
    , _pos        = Pos 0 0
    , _vel        = Vel 0 0
    , _entityData = d
}

velAdd :: Entity a -> Entity a
velAdd e = over (pos.pY) (+ view (vel.vY) e) e

velSub :: Entity a -> Entity a
velSub e = over (pos.pY) (\x -> x - view (vel.vY) e) e