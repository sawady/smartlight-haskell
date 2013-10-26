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
    _mousePos   :: Pos,
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
    
loadImage :: String -> Game a -> IO (Game a)
loadImage n g = do
    img <- loadImageResource n
    return $ addImage n img g    
    
loadImages :: [String] -> Game a -> IO (Game a)
loadImages xs g = foldM (flip loadImage) g xs

addFont :: String -> Font -> Game a -> Game a
addFont n fnt = over fonts (Map.insert n fnt)

getFont :: String -> Game a -> Font
getFont n g = fromMaybe (error $ "Font " ++ n ++ " not found") $
    Map.lookup n (view fonts g)
    
loadFont :: String -> Int -> Game a -> IO (Game a)
loadFont n s g = do
    fnt <- loadFontResource n s
    return $ addFont n fnt g
    
loadFonts :: [(String,Int)] -> Game a -> IO (Game a)
loadFonts xs g = foldM (flip . uncurry $ loadFont) g xs

drawEntity :: Entity a -> Game b -> IO ()
drawEntity e = drawImage (view pos e) (view entityName e)

drawEntities :: [Entity ()] -> Game a -> IO ()
drawEntities es g = mapM_ (`drawEntity` g) es 

drawGameEntity :: Getter b (Entity a) -> Game b -> IO ()
drawGameEntity p g = drawEntity (view p (view gameData g)) g

drawGameEntityBounds :: Getter b (Entity a) -> Game b -> IO ()
drawGameEntityBounds p g = drawEntityBounds e g
    where e = view (gameData.p) g
    
drawEntityBounds :: Entity a -> Game b -> IO () 
drawEntityBounds e = drawOnScreen (rectangle (boundsRect e) (255, 0, 0, 255))
    
drawEntitiesBounds :: [Entity ()] -> Game a -> IO ()
drawEntitiesBounds es g = mapM_ (`drawEntityBounds` g) es     

drawImage :: Pos -> String -> Game a -> IO ()
drawImage p img g = drawOnScreen (drawImageOnSurface p (getImage img g)) g

drawText :: Show t => Pos -> t -> String -> Color -> Game a -> IO ()
drawText p text fnt c g = drawOnScreen (drawTextOnSurface p (show text) (getFont fnt g) c) g

drawOnScreen :: (Surface -> IO ()) -> Game a -> IO ()
drawOnScreen f g = f $ view (screen.screenSurface) g

toVoidGameEntity ::  Getter b (Entity a) -> Game b -> Entity ()
toVoidGameEntity p g = toVoidEntity $ view (gameData.p) g

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