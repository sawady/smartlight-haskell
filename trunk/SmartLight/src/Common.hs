{-# LANGUAGE RankNTypes, KindSignatures, FlexibleContexts #-}
module Common where

import Control.Lens

type Pos = (Int,Int)
type Vel = (Int,Int)

_x :: forall s t a b (f :: * -> *) (p :: * -> * -> *) .
        (Functor f, Indexable Int p, Field1 s t a b) =>
        p a (f b) -> s -> f t
_x = _1

_y :: forall s t a b (f :: * -> *) (p :: * -> * -> *).
        (Functor f, Indexable Int p, Field2 s t a b) =>
        p a (f b) -> s -> f t
_y = _2
