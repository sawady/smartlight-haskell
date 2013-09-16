module Ball where

import SmartLight
import CommonPong

type Ball = PureEntity

newBall :: Ball
newBall = (newPureEntity "ball") {
      _vel = Vel 3 3
}

moveBall :: Ball -> Ball
moveBall = velAdd . bounceOnEdgeX screenSizeX . bounceOnEdgeY screenSizeY