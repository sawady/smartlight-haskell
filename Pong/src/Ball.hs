module Ball where

import SmartLight
import CommonPong

type Ball = Entity ()

newBall :: Ball
newBall = (newVoidEntity "ball") {
    _pos = midScreen 
  , _vel = (3, 3)
  , _bounds = (23,23)
}

moveBall :: Procedure Ball
moveBall = velAdd