module Entity where

import Image
import qualified Data.HashMap.Strict as Map (HashMap, empty)

data GameEntity = GameEntity {
    _entityId   :: Int,
    _entityData :: EntityData
}

data EntityData = EntityData {
    _currentImage :: Maybe Image,
    _images  :: Map.HashMap String Image
}

newEntityData :: EntityData
newEntityData = EntityData {
    _currentImage = Nothing,
    _images = Map.empty
}
