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


type Position = { x :: Number, y :: Number }

pos :: Number -> Number -> Position
pos = { x: _, y: _ }

data City = City String Position

tennenlohe :: City
tennenlohe = City "Tennenlohe" (pos 0.2 0.5)

leipzig :: City
leipzig = City "Leipzig" (pos 0.7 0.6)

cities :: Array City
cities = [ tennenlohe, leipzig ]

connections :: Array (Tuple City City)
connections = [ Tuple tennenlohe leipzig ]


showPercent :: Number -> String
showPercent n = (show (n * 100.0)) <> "%"


data Query a = MovePlayer City a

type UiState = { playerPosition :: City }

initialState :: UiState
initialState = { playerPosition: tennenlohe }

ui :: forall g. H.Component UiState Query g
ui = H.component { render, eval }
  where

  svgns = Just (namespace "http://www.w3.org/2000/svg")
  svgAttr name value = Attr Nothing (attrName name) value

  svgCity city@(City cityName { x, y }) =
    [ Element svgns (tagName "circle") [ svgAttr "class" "city"
                                       , svgAttr "r" "10"
                                       , svgAttr "cx" (showPercent x)
                                       , svgAttr "cy" (showPercent y)
                                       , onClick (HE.input_ (MovePlayer city))
                                       ] []
    , Element svgns (tagName "text") [ svgAttr "class" "city-label"
                                     , svgAttr "x" (showPercent x)
                                     , svgAttr "y" (showPercent (y + 0.02))
                                     , svgAttr "text-anchor" "middle"
                                     , svgAttr "alignment-baseline" "central"
                                     , onClick (HE.input_ (MovePlayer city))
                                     ] [ HH.text cityName ]
    ]

  svgConnection (Tuple (City cn1 { x: x1, y: y1 }) (City cn2 { x: x2, y: y2 })) =
    Element svgns (tagName "line") [ svgAttr "class" "connection"
                                   , svgAttr "x1" (showPercent x1)
                                   , svgAttr "y1" (showPercent y1)
                                   , svgAttr "x2" (showPercent x2)
                                   , svgAttr "y2" (showPercent y2)
                                   ] []

  svgPlayer { x, y } =
    Element svgns (tagName "circle") [ svgAttr "class" "player"
                                     , svgAttr "r" "12"
                                     , svgAttr "cx" (showPercent x)
                                     , svgAttr "cy" (showPercent y)
                                     ] []

  render :: UiState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ Element svgns (tagName "svg") []
          ((map svgConnection connections) <> (cities >>= svgCity) <>
          [ svgPlayer (case state.playerPosition of City cityName position -> position)
          ])
      ]

  eval :: Query ~> H.ComponentDSL UiState Query g
  eval (MovePlayer city next) = do
    H.modify (\state -> { playerPosition: city })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
