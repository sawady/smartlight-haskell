{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Screen
import Image
import Data.Lens.Template
import Data.Lens.Common
import qualified Data.HashMap.Strict as Map
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

data Game gameData = Game {
    _isRunning  :: Bool,
    _screen     :: Screen,
    _entities   :: Map.HashMap String Image,
    _gameData   :: gameData
}

$( makeLenses [''Game] )

addEntity :: String -> Image -> Game a -> Game a
addEntity n e = entities ^%= Map.insert n e 

removeEntity :: String -> Game a -> Game a
removeEntity e = entities ^%= Map.delete e

getEntity :: String -> Game a -> Image
getEntity n g = fromMaybe (error $ "Image " ++ n ++ " not found") $
    Map.lookup n (_entities g)
    
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
    _screen     = s,
    _isRunning  = True,
    _entities   = Map.empty,
    _gameData   = g
}

finish :: Game a -> Game a
finish = isRunning ^= False