module Config where

import Prelude
import Data.Tuple (Tuple(..))


type Position = { x :: Number, y :: Number }

data City = City String Position
instance eqCity :: Eq City
  where eq (City cn1 _) (City cn2 _) = cn1 == cn2

type Config = { cities :: Array City
              , playerStart :: City
              , connections :: Array (Tuple City City)
              }

config :: Config
config = { cities: [ atlanta, chicago, miami ]
         , playerStart: atlanta
         , connections: [ Tuple atlanta chicago
                        , Tuple atlanta miami
                        ]
         }
  where
  atlanta = City "Atlanta" (pos 0.21 0.35)
  chicago = City "Chicago" (pos 0.19 0.28)
  miami = City "Miami" (pos 0.25 0.43)

  pos x y = { x, y }
