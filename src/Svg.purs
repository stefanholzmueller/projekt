module Svg where

import Prelude
import Config as C
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

svgCity :: forall i p. C.City -> (Event MouseEvent -> EventHandler (Maybe i)) -> Boolean -> Array (HTML p i)
svgCity (C.City cityName { x, y }) event isReachable =
  [ Element svgns (tagName "circle") ([ svgAttr "class" ("city" <> reachableSuffix)
                                      , svgAttr "r" "10"
                                      , svgAttr "cx" (showPercent x)
                                      , svgAttr "cy" (showPercent y)
                                      ] <> eventHandlers) []
  , Element svgns (tagName "text") ([ svgAttr "class" ("city-label" <> reachableSuffix)
                                    , svgAttr "x" (showPercent x)
                                    , svgAttr "y" (showPercent (y + 0.03))
                                    , svgAttr "text-anchor" "middle"
                                    , svgAttr "alignment-baseline" "central"
                                    ] <> eventHandlers) [ text cityName ]
  ]
  where
    reachableSuffix = if isReachable then " reachable" else ""
    eventHandlers = if isReachable then [ onClick event ] else []

svgConnection :: forall i p. C.Connection -> Array (HTML p i)
svgConnection { between: (Tuple (C.City cn1 pos1) (C.City cn2 pos2)), connectionType } =
  case connectionType of
    C.NormalConnection -> [ svgLine pos1.x pos1.y pos2.x pos2.y
                          ]
    C.WrapsAroundMap -> [ svgLine pos1.x pos1.y 0.0 y1'
                        , svgLine pos2.x pos2.y 1.0 y2'
                        ]
  where
  svgLine x1 y1 x2 y2 = Element svgns (tagName "line") [ svgAttr "class" "connection"
                                                       , svgAttr "x1" (showPercent x1)
                                                       , svgAttr "y1" (showPercent y1)
                                                       , svgAttr "x2" (showPercent x2)
                                                       , svgAttr "y2" (showPercent y2)
                                                       ] []
  dx = pos2.x - pos1.x
  dy = pos2.y - pos1.y
  y1' = pos1.y + dy * pos1.x / (1.0 - dx)
  y2' = pos1.x + dx * pos1.y / (1.0 - dy)

svgPlayer :: forall i p. C.Position -> HTML p i
svgPlayer { x, y } =
  Element svgns (tagName "circle") [ svgAttr "class" "player"
                                   , svgAttr "r" "12"
                                   , svgAttr "cx" (showPercent x)
                                   , svgAttr "cy" (showPercent y)
                                   ] []
