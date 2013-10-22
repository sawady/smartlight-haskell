{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Game where

import Graphics.UI.SDL.TTF
import Graphics.UI.SDL (Event, Event(NoEvent), Color, Surface)
import Screen
import Image
import Text
import Common
import Entity
import Control.Lens.TH
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Word
import Draw

data Game gameData = Game {
    _isRunning  :: Bool,
    _mousePos   :: (Int,Int),
    _event      :: Event,
    _fps        :: Word32,
    _screen     :: Screen,
    _images     :: Map.HashMap String Image,
    _fonts      :: Map.HashMap String Font,
    _gameData   :: gameData
}

makeLenses ''Game

addImage :: String -> Image -> Game a -> Game a
addImage n e = over images (Map.insert n e)

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

addFont :: String -> Font -> Game a -> Game a
addFont n fnt = over fonts (Map.insert n fnt)

getFont :: String -> Game a -> Font
getFont n g = fromMaybe (error $ "Font " ++ n ++ " not found") $
    Map.lookup n (view fonts g)
    
loadToFonts :: String -> Int -> Game a -> IO (Game a)
loadToFonts n s g = do
    fnt <- loadFont n s
    return $ addFont n fnt g
    
loadFontResources :: [(String,Int)] -> Game a -> IO (Game a)
loadFontResources xs g = foldM (flip . uncurry $ loadToFonts) g xs

newGameEntity :: Game a -> b -> String -> Entity b
newGameEntity g d n = (newEntity d n) {
        _bounds = uncurry (,) $ imgSize (getImage n g)
    }  

drawEntity :: Getter b (Entity a) -> Game b -> IO ()
drawEntity p g = drawEntity' (view p (view gameData g)) g

drawEntityBounds ::Getter b (Entity a) -> Game b -> IO ()
drawEntityBounds p g = drawOnScreen (rectangle (boundsRect e) (255,0,0,255)) g
    where e = view (gameData.p) g

drawEntity' :: Entity a -> Game b -> IO ()
drawEntity' e = drawImage (view (pos . _x) e) (view (pos . _y) e) (view entityName e)

drawImage :: Int -> Int -> String -> Game a -> IO ()
drawImage x y img g = drawImageOnSurface x y (getImage img g) (_screenSurface (_screen g))

drawText :: Show t => Int -> Int -> t -> String -> Color -> Game a -> IO ()
drawText x y text fnt c g = drawTextOnSurface x y (show text) (getFont fnt g) c (_screenSurface (_screen g))

drawOnScreen :: (Surface -> IO ()) -> Game a -> IO ()
drawOnScreen f g = f $ view (screen.screenSurface) g

newGame :: Screen -> a -> Game a
newGame s g = Game {
      _screen         = s
    , _mousePos       = (0,0)
    , _event          = NoEvent  
    , _isRunning      = True
    , _fonts          = Map.empty
    , _images         = Map.empty
    , _gameData       = g
    , _fps            = 60
}

finish :: Game a -> Game a
finish = set isRunning False