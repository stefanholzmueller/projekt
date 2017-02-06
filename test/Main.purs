module Test.Main where

import Prelude
import Test.Game as TG
import Test.StrongCheck (SC)

main :: forall eff. SC eff Unit
main = do
  TG.main
