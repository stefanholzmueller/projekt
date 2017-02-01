module Main where

import Prelude
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML.Core (HTML(..), Prop(..), attrName, namespace, tagName)
import Halogen.HTML.Events (onClick)
import Halogen.Util (awaitBody, runHalogenAff)


data Position = Position String String
data City = City String Position

tennenlohe :: City
tennenlohe = City "Tennenlohe" (Position "20%" "50%")

leipzig :: City
leipzig = City "Leipzig" (Position "70%" "60%")

cities :: Array City
cities = [ tennenlohe, leipzig ]

connections :: Array (Tuple City City)
connections = [ Tuple tennenlohe leipzig ]


data Query a = MovePlayer City a

type State = { playerPosition :: City }

initialState :: State
initialState = { playerPosition: tennenlohe }

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where

  svgns = Just (namespace "http://www.w3.org/2000/svg")
  svgAttr name value = Attr Nothing (attrName name) value

  svgCity city@(City cityName (Position x y)) =
    Element svgns (tagName "circle") [ svgAttr "class" "city"
                                     , svgAttr "r" "10"
                                     , svgAttr "cx" x
                                     , svgAttr "cy" y
                                     , onClick (HE.input_ (MovePlayer city))
                                     ] []

  svgConnection (Tuple (City cn1 (Position x1 y1)) (City cn2 (Position x2 y2))) =
    Element svgns (tagName "line") [ svgAttr "class" "connection"
                                   , svgAttr "x1" x1
                                   , svgAttr "y1" y1
                                   , svgAttr "x2" x2
                                   , svgAttr "y2" y2
                                   ] []

  svgPlayer (Position x y) =
    Element svgns (tagName "circle") [ svgAttr "class" "player"
                                     , svgAttr "r" "12"
                                     , svgAttr "cx" x
                                     , svgAttr "cy" y
                                     ] []

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ Element svgns (tagName "svg") []
          ((map svgConnection connections) <> (map svgCity cities) <>
          [ svgPlayer (case state.playerPosition of City cityName position -> position)
          ])
      ]

  eval :: Query ~> H.ComponentDSL State Query g
  eval (MovePlayer city next) = do
    H.modify (\state -> { playerPosition: city })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
