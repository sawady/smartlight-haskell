{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Graphics.UI.SDL (Event, Event(NoEvent))
import Screen
import Image
import Bounds
import Entity
import Control.Lens.TH
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Word

data Game gameData = Game {
    _isRunning  :: Bool,
    _mousePos   :: (Int,Int),
    _event      :: Event,
    _fps        :: Word32,
    _lastUpdateTime :: Word32,
    _screen         :: Screen,
    _images         :: Map.HashMap String Image,
    _gameData       :: gameData
}

makeLenses ''Game

addImage :: String -> Image -> Game a -> Game a
addImage n e = over images (Map.insert n e)

removeImage :: String -> Game a -> Game a
removeImage n = over images (Map.delete n)

getImage :: String -> Game a -> Image
getImage n g = fromMaybe (error $ "Image " ++ n ++ " not found") $
    Map.lookup n (view images g)
    
loadToImages :: String -> Game a -> IO (Game a)
loadToImages n g = do
    img <- loadImage n
    r   <- getArea img
    return $ addImage n (newImage img r) g

loadImageResources :: [String] -> Game a -> IO (Game a)
loadImageResources xs g = foldM (flip loadToImages) g xs

drawEntity :: forall a b.
                Getting (Entity a) b (Entity a) -> Game b -> IO ()
drawEntity p g = drawEntity' (view p (view gameData g)) g

drawEntity' :: Entity a -> Game b -> IO ()
drawEntity' e = drawImage (view (pos . pX) e) (view (pos . pY) e) (view entityName e)

drawImage :: Int -> Int -> String -> Game a -> IO ()
drawImage x y img g = drawImageOnSurface x y (getImage img g) (_screenSurface (_screen g))

newGame :: Screen -> a -> Game a
newGame s g = Game {
      _screen         = s
    , _lastUpdateTime = 0
    , _mousePos       = (0,0)
    , _event          = NoEvent  
    , _isRunning      = True
    , _images         = Map.empty
    , _gameData       = g
    , _fps            = 60
}

finish :: Game a -> Game a
finish = set isRunning False

mouseX,mouseY :: Game a -> Int
mouseX g = view (mousePos . _1) g + (view (screen . screenData . width)  g `div` 2)
mouseY g = view (mousePos . _2) g - (view (screen . screenData . height) g `div` 2)

collideWith :: Entity a -> Entity b -> Game c -> Bool
collideWith e1 e2 g = collideWithBounds rImg1 rImg2
    where img1 = getImage (view entityName e1) g
          img2 = getImage (view entityName e2) g
          rImg1   = RBound . uncurry (RectBound (view pos e1)) $ imgSize img1
          rImg2   = RBound . uncurry (RectBound (view pos e2)) $ imgSize img2