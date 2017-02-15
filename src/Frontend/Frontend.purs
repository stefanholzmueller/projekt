module Frontend where

import Prelude
import Backend.Config as C
import Control.Monad.Eff (Eff)
import Backend.Game as G
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.Util as HU
import Frontend.Svg as S


config :: C.Config
config = C.config


data Query a = MovePlayer C.City a

type UiState = { playerPosition :: C.City }

initialState :: UiState
initialState = { playerPosition: config.playerStart }

ui :: forall g. H.Component UiState Query g
ui = H.component { render, eval }
  where

  render :: UiState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ S.svg [] (svgImages <> svgConnections <> svgCities <> svgPlayers state.playerPosition)
      ]
    where
    svgImages = [ S.svgWorldMap ]
    svgPlayers (C.City _ position) = [ S.svgPlayer position ]
    svgConnections = config.connections >>= S.svgConnection
    svgCities = config.cities >>= renderCity
    renderCity city = S.svgCity city (HE.input_ (MovePlayer city)) (G.isReachable config.connections state.playerPosition city)

  eval :: Query ~> H.ComponentDSL UiState Query g
  eval (MovePlayer city next) = do
    H.modify (\state -> { playerPosition: city })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = HU.runHalogenAff do
  body <- HU.awaitBody
  H.runUI ui initialState body
