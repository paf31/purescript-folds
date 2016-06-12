module Test.Main where

import Prelude
import Control.Fold (Fold, scanl, length, sum)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow (scanl average [1.0, 2.0, 3.0, 4.0, 5.0])
  where
    average :: forall s. EuclideanRing s => Fold s s
    average = (/) <$> sum <*> length
