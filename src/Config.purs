module Config where

import Data.Tuple (Tuple(..))


type Position = { x :: Number, y :: Number }

pos :: Number -> Number -> Position
pos x y = { x, y }

data City = City String Position

tennenlohe :: City
tennenlohe = City "Tennenlohe" (pos 0.2 0.5)

leipzig :: City
leipzig = City "Leipzig" (pos 0.7 0.6)

cities :: Array City
cities = [ tennenlohe, leipzig ]

connections :: Array (Tuple City City)
connections = [ Tuple tennenlohe leipzig ]
