module Backend.Config where

import Prelude
import Data.Tuple (Tuple(..))


type Position = { x :: Number, y :: Number }

data City = City String Position
instance eqCity :: Eq City
  where eq (City cn1 _) (City cn2 _) = cn1 == cn2
instance showCity :: Show City
  where show (City cn _) = "City:" <> cn
newCity :: String -> Number -> Number -> City
newCity name x y = City name { x, y }

data ConnectionType = NormalConnection | WrapsAroundMap
derive instance eqConnectionType :: Eq ConnectionType
type Connection = { between :: Tuple City City
                  , connectionType :: ConnectionType
                  }
newConnection :: City -> City -> ConnectionType -> Connection
newConnection city1 city2 connectionType = { between: Tuple city1 city2, connectionType }

type Config = { cities :: Array City
              , playerStart :: City
              , connections :: Array Connection
              }

config :: Config
config = { cities: [ atlanta, chicago, miami, sanFrancisco, manila ]
         , playerStart: atlanta
         , connections: [ newConnection atlanta chicago NormalConnection
                        , newConnection atlanta miami NormalConnection
                        , newConnection chicago sanFrancisco NormalConnection
                        , newConnection sanFrancisco manila WrapsAroundMap
                        ]
         }
  where
  atlanta = newCity "Atlanta" 0.21 0.35
  chicago = newCity "Chicago" 0.19 0.28
  miami = newCity "Miami" 0.25 0.43
  sanFrancisco = newCity "San Francisco" 0.10 0.31
  manila = newCity "Manila" 0.87 0.57

  pos x y = { x, y }
