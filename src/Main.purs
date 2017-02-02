module Main where

import Prelude
import Config as C
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.Util as HU
import Svg as S


data Query a = MovePlayer C.City a

type UiState = { playerPosition :: C.City }

initialState :: UiState
initialState = { playerPosition: C.tennenlohe }

ui :: forall g. H.Component UiState Query g
ui = H.component { render, eval }
  where

  svgConnections = map S.svgConnection C.connections
  svgCities = C.cities >>= (\city -> S.svgCity city (HE.input_ (MovePlayer city)))
  svgPlayers (C.City _ position) = [ S.svgPlayer position ]

  render :: UiState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ S.svg [] (svgConnections <> svgCities <> svgPlayers state.playerPosition)
      ]

  eval :: Query ~> H.ComponentDSL UiState Query g
  eval (MovePlayer city next) = do
    H.modify (\state -> { playerPosition: city })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = HU.runHalogenAff do
  body <- HU.awaitBody
  H.runUI ui initialState body
