{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Graphics.UI.SDL (Event, Event(NoEvent))
import Screen
import Image
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
    _screen     :: Screen,
    _entities   :: Map.HashMap String Image,
    _gameData   :: gameData
}

makeLenses ''Game

addEntity :: String -> Image -> Game a -> Game a
addEntity n e = over entities (Map.insert n e) 

removeEntity :: String -> Game a -> Game a
removeEntity n = over entities (Map.delete n)

getEntity :: String -> Game a -> Image
getEntity n g = fromMaybe (error $ "Image " ++ n ++ " not found") $
    Map.lookup n (view entities g)
    
loadEntity :: String -> Game a -> IO (Game a)
loadEntity n g = do
    img <- loadImage n
    r   <- getArea img
    return $ addEntity n (newImage img r) g

loadEntities :: [String] -> Game a -> IO (Game a)
loadEntities xs g = foldM (flip loadEntity) g xs


drawEntity :: Int -> Int -> String -> Game a -> IO ()
drawEntity x y img g = drawImage x y (getEntity img g) (_screenSurface (_screen g))

newGame :: Screen -> a -> Game a
newGame s g = Game {
      _screen         = s
    , _lastUpdateTime = 0
    , _mousePos       = (0,0)
    , _event          = NoEvent  
    , _isRunning      = True
    , _entities       = Map.empty
    , _gameData       = g
    , _fps            = 60
}

finish :: Game a -> Game a
finish = set isRunning False

mouseX,mouseY :: Game a -> Int
mouseX g = view (mousePos . _1) g + (view (screen . screenData . width)  g `div` 2)
mouseY g = view (mousePos . _2) g - (view (screen . screenData . height) g `div` 2)