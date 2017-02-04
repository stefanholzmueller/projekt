module Game where

import Prelude
import Config (City)
import Data.Array (any)
import Data.Tuple (Tuple(..))


isReachable :: Array (Tuple City City) -> City -> City -> Boolean
isReachable connections position target = any areConnected connections
  where
  areConnected (Tuple city1 city2) = position == city1 && target == city2 || position == city2 && target == city1
