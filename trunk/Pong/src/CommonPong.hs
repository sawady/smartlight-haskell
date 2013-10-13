module CommonPong where

screenSize :: (Int, Int)
screenSize = (640, 480)

midScreen :: (Int, Int)
midScreen = let (x,y) = screenSize
                in (x `div` 2, y `div` 2)  