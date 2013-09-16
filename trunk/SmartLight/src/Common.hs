{-# LANGUAGE TemplateHaskell #-}
module Common where

import Control.Lens.TH

data Pos = Pos {
      _pX :: Int
    , _pY :: Int
}

data Vel = Vel {
      _vX :: Int
    , _vY :: Int
}

makeLenses ''Pos
makeLenses ''Vel

