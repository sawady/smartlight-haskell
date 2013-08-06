{-# LANGUAGE TemplateHaskell #-}
module GameEntity where

import Image
import qualified Data.HashMap.Strict as Map
import Data.Lens.Template
import Data.Lens.Common
import Common

type ImageIndex = String

data GameEntity = GameEntity {
    _entityName   :: String,
    _currentImage :: Maybe ImageIndex,
    _images       :: Map.HashMap String Image
}

instance Eq GameEntity where
    (==) g1 g2 = _entityName g1 == _entityName g2

instance Ord GameEntity where
    (>) g1 g2 = _entityName g1 > _entityName g2

$( makeLenses [''GameEntity] )

newEntity :: String -> GameEntity
newEntity s = GameEntity {
    _entityName   = s,
    _currentImage = Nothing,
    _images = Map.empty
}

singleImageEntity :: ImageIndex -> Image -> String -> GameEntity
singleImageEntity i img n = setCurrentImage i . addImage i img $ newEntity n

setCurrentImage :: String -> GameEntity -> GameEntity
setCurrentImage n = currentImage ^= Just n
    
addImage :: String -> Image -> GameEntity -> GameEntity
addImage = (images ^%=) `compose2` Map.insert
