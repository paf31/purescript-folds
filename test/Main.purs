module Test.Main where

import Prelude
import Control.Fold (Fold, scanl, length, sum)
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = logShow (scanl average [1.0, 2.0, 3.0, 4.0, 5.0])
  where
    average :: forall s. EuclideanRing s => Fold s s
    average = (/) <$> sum <*> length
