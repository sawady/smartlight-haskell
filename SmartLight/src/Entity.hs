{-# LANGUAGE TemplateHaskell #-}
module Entity where

import Image
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap as IntMap
import Data.Lens.Template

data EntityManager = EntityManager {
    _entities     :: IntMap.IntMap GameEntity
}

data GameEntity = GameEntity {
    _entityId   :: Int,
    _entityType :: EntityType
}

data EntityType = Single EntityData | Group [GameEntity]

data EntityData = EntityData {
    _currentImage :: Maybe Image,
    _images       :: Map.HashMap String Image
}

$( makeLenses [''EntityManager, ''GameEntity, ''EntityData] )

newEntityManager :: EntityManager
newEntityManager = EntityManager {
    _entities     = IntMap.empty
}

addEntity :: EntityType -> EntityManager -> EntityManager
addEntity e g = g {
    _entities = IntMap.insert newKey (GameEntity newKey e) (_entities g)
}
    where
        newKey = if null entKeys then 1 else maximum entKeys
        entKeys = IntMap.keys (_entities g)

removeEntity :: GameEntity -> EntityManager -> EntityManager
removeEntity e g = g {
    _entities = IntMap.delete (_entityId e) (_entities g)
}

newEntityData :: EntityData
newEntityData = EntityData {
    _currentImage = Nothing,
    _images = Map.empty
}
