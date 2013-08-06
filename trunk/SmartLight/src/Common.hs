module Common(compose2, module X) where

import Data.Lens.Common as X

compose2 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
compose2 = (.) . (.)

