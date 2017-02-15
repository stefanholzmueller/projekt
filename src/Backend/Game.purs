module Backend.Game where

import Prelude
import Backend.Config as C
import Data.Array (any)
import Data.Tuple (Tuple(..))


isReachable :: Array C.Connection -> C.City -> C.City -> Boolean
isReachable connections position target = any areDirectlyConnected (map _.between connections)
  where
  areDirectlyConnected (Tuple city1 city2) = position == city1 && target == city2 || position == city2 && target == city1
