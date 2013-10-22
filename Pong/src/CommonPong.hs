module CommonPong where

import Control.Lens

screenSize :: (Int, Int)
screenSize = (640, 480)

midScreen :: (Int, Int)
midScreen = over _1 (`div` 2) $ 
            over _2 (`div` 2)  
            screenSize  