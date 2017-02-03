module Config where

import Prelude
import Data.Tuple (Tuple(..))


type Position = { x :: Number, y :: Number }

data City = City String Position
derive instance eqCity :: Eq City

type Config = { cities :: Array City
              , playerStart :: City
              , connections :: Array (Tuple City City)
              }

config :: Config
config = { cities: [ atlanta, miami, chicago ]
         , playerStart: atlanta
         , connections: [ Tuple atlanta miami
                        , Tuple atlanta chicago
                        ]
         }
  where
  atlanta = City "Atlanta" (pos 0.21 0.35)
  miami = City "Miami" (pos 0.25 0.43)
  chicago = City "Chicago" (pos 0.19 0.28)

  pos x y = { x, y }
