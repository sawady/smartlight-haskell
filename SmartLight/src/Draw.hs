module Draw where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import Data.Bits

import Common
type RGBA = (Int,Int,Int,Int)

toPixel :: RGBA -> Pixel
toPixel (r,g,b,a) = Pixel $ shiftL r' 24 .|. shiftL g' 16 .|. shiftL b' 8 .|. a'
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b
          a' = fromIntegral a
          
void :: IO a -> IO ()
void ac = do
    _ <- ac
    return ()          

pixel :: Pos -> RGBA -> Surface -> IO ()
pixel (x, y) c s = void $ SDL.pixel s (fromIntegral x) (fromIntegral y) (toPixel c)

hLine :: Pos -> Int -> RGBA -> Surface -> IO ()
hLine (x, y) e c s = void $ SDL.hLine s (fromIntegral x) (fromIntegral y) (fromIntegral e) (toPixel c)

--vLine :: Int -> Int -> Int -> RGBA -> Surface -> IO ()
--

rectangle :: Rect -> RGBA -> Surface -> IO ()
rectangle r c s = void $ SDL.rectangle s r (toPixel c)

circle :: Pos -> Int -> RGBA -> Surface -> IO ()
circle (x,y) r c s = void $ SDL.circle s (fromIntegral x) (fromIntegral y) (fromIntegral r) (toPixel c)

--box :: Int -> RGBA -> Surface -> IO ()
--
--line :: Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--aaLine :: Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--arc :: Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--aaCircle :: Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--filledCircle :: Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--ellipse :: Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--aaEllipse :: Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--filledEllipse :: Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--pie :: Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--filledPie :: Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--trigon :: Int -> Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--filledTrigon :: Int -> Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--aaTrigon :: Int -> Int -> Int -> Int -> Int -> Int -> RGBA -> Surface -> IO ()
--
--polygon :: [(Int, Int)] -> RGBA -> Surface -> IO ()
--
--texturedPolygon :: [(Int, Int)] -> Surface -> Int -> Int -> Surface -> IO ()
--
--filledPolygon :: [(Int, Int)] -> RGBA -> Surface -> IO ()
--
--aaPolygon :: [(Int, Int)] -> RGBA -> Surface -> IO ()
--
--bezier :: [(Int, Int)] -> Int -> RGBA -> Surface -> IO ()