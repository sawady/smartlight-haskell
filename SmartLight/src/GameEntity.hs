{-# LANGUAGE TemplateHaskell #-}
module GameEntity where

import Image
import qualified Data.IntMap as Map
import Data.Lens.Template
import Data.Lens.Common
import Data.Maybe
import Common

data GameEntity = GameEntity {
    _currentImage :: Int,
    _images       :: Map.IntMap Image
} deriving (Show)

$( makeLenses [''GameEntity] )

getCurrentImage :: GameEntity -> Image
getCurrentImage g = fromJust $ Map.lookup (_currentImage g) (_images g)

singleImageEntity :: Image -> GameEntity
singleImageEntity img = GameEntity {
    _currentImage = 0,
    _images = Map.insert 0 img Map.empty
}

setCurrentImage :: Int -> GameEntity -> GameEntity
setCurrentImage n = currentImage ^= n
    
addImage :: Int -> Image -> GameEntity -> GameEntity
addImage = (images ^%=) `compose2` Map.insert
