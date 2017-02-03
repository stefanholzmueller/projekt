module Svg where

import Prelude
import Config (City(..), Position)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML.Core (HTML(..), Namespace, Prop(..), attrName, namespace, tagName)
import Halogen.HTML.Events (Event, onClick)
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.HTML.Events.Types (MouseEvent)
import Halogen.HTML.Indexed (text)


showPercent :: Number -> String
showPercent n = (show (n * 100.0)) <> "%"

svgns :: Maybe Namespace
svgns = Just (namespace "http://www.w3.org/2000/svg")

svgAttr :: forall i. String -> String -> Prop i
svgAttr name value = Attr Nothing (attrName name) value


svg :: forall i p. Array (Prop i) -> Array (HTML p i) -> HTML p i
svg = Element svgns (tagName "svg")

svgWorldMap :: forall i p. HTML p i
svgWorldMap =
  Element svgns (tagName "image") [ svgAttr "class" "world-map"
                                  , svgAttr "href" "worldmap.jpg"
                                  ] []

svgCity :: forall i p. City -> (Event MouseEvent -> EventHandler (Maybe i)) -> Array (HTML p i)
svgCity (City cityName { x, y }) event =
  [ Element svgns (tagName "circle") [ svgAttr "class" "city"
                                     , svgAttr "r" "10"
                                     , svgAttr "cx" (showPercent x)
                                     , svgAttr "cy" (showPercent y)
                                     , onClick event
                                     ] []
  , Element svgns (tagName "text") [ svgAttr "class" "city-label"
                                   , svgAttr "x" (showPercent x)
                                   , svgAttr "y" (showPercent (y + 0.02))
                                   , svgAttr "text-anchor" "middle"
                                   , svgAttr "alignment-baseline" "central"
                                   , onClick event
                                   ] [ text cityName ]
  ]

svgConnection :: forall i p. Tuple City City -> HTML p i
svgConnection (Tuple (City cn1 { x: x1, y: y1 }) (City cn2 { x: x2, y: y2 })) =
  Element svgns (tagName "line") [ svgAttr "class" "connection"
                                 , svgAttr "x1" (showPercent x1)
                                 , svgAttr "y1" (showPercent y1)
                                 , svgAttr "x2" (showPercent x2)
                                 , svgAttr "y2" (showPercent y2)
                                 ] []

svgPlayer :: forall i p. Position -> HTML p i
svgPlayer { x, y } =
  Element svgns (tagName "circle") [ svgAttr "class" "player"
                                   , svgAttr "r" "12"
                                   , svgAttr "cx" (showPercent x)
                                   , svgAttr "cy" (showPercent y)
                                   ] []
