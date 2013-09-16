{-# LANGUAGE TemplateHaskell #-}
module Common where

import Control.Monad ((>=>))
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

compose2 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
compose2 = (.) . (.)

extendsM :: Monad m => (t -> c -> m c) -> t -> t -> c -> m c
extendsM method g h = method g >=> method h

extendsM_ :: Monad m => (t -> c -> m ()) -> t -> t -> c -> m ()
extendsM_ method g h e = method g e >> method h e

extends :: (t -> c -> c) -> t -> t -> c -> c
extends method g h = method g . method h