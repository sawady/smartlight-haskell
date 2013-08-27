module Ball where

import SmartLight 

import Control.Lens
import CommonPong

type Ball = PureEntity

newBall :: Ball
newBall = (newPureEntity "ball") {
      _vel = Vel 3 3
}

moveBall :: Ball -> Ball
moveBall = moveBallX . moveBallY

moveBallX :: Ball -> Ball
moveBallX b = over (pos . pX) (+ view (vel.vX) b) (bounceX b)
               
moveBallY :: Ball -> Ball
moveBallY b = over (pos . pY) (+ view (vel.vY) b) (bounceY b)
               
bounceX :: Ball -> Ball
bounceX b = if onEdgeX (view (pos . pX) b) 
               then (\b' -> over (pos . pX) (+ view (vel . vX) b' * 2) b') . over (vel  . vX) (* (-1)) $ b
               else b

bounceY :: Ball -> Ball
bounceY b = if onEdgeY (view (pos . pY) b) 
               then (\b' -> over (pos . pY) (+ view (vel . vY) b' * 2) b') . over (vel  . vY) (* (-1)) $ b
               else b

onEdgeX :: Int -> Bool
onEdgeX x = x < (- screenSizeX `div` 2) || x > (screenSizeX `div` 2)
               
onEdgeY :: Int -> Bool
onEdgeY y = y < (- screenSizeY `div` 2) || y > (screenSizeY `div` 2)