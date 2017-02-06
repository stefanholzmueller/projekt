module Test.Game where

import Prelude
import Game as G
import Config (City(..))
import Data.Array ((!!), length)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.StrongCheck (class Arbitrary, Result, SC, assertEq, quickCheck, (<?>))
import Test.StrongCheck.Gen (GenT, chooseInt)


-- | Creates a generator that chooses an random element from among a non-empty array of elements.
sample1 :: forall f a. Monad f => Array a -> GenT f a
sample1 xs = do
  i <- chooseInt 0 ((length xs) - 1)
  pure (unsafePartial (fromJust (xs !! i)))


makeCity :: Int -> City
makeCity n = City ("City" <> show n) { x: 0.0, y: 0.0 }

cities :: Array City
cities = [ makeCity 1, makeCity 2, makeCity 3 ]

connections :: Array (Tuple City City)
connections = [ Tuple (makeCity 1) (makeCity 2)
              , Tuple (makeCity 3) (makeCity 2)
              ]

newtype RandomConnection = RandomConnection (Tuple City City)
instance arbConnection :: Arbitrary RandomConnection
  where
  arbitrary = do
    conn <- sample1 connections
    pure (RandomConnection conn)

newtype TwoRandomCities = TwoRandomCities (Tuple City City)
instance arbTwoRandomCities :: Arbitrary TwoRandomCities
  where
  arbitrary = do
    city1 <- sample1 cities
    city2 <- sample1 cities
    pure (TwoRandomCities (Tuple city1 city2))


main :: forall eff. SC eff Unit
main = do
  quickCheck twoCitiesFromConnectionsAreReachable
  quickCheck twoRandomCitiesAreReachableOrNotButSymmetric


twoCitiesFromConnectionsAreReachable :: RandomConnection -> Result
twoCitiesFromConnectionsAreReachable (RandomConnection (Tuple city1 city2)) =
  (G.isReachable connections city2 city1) <?> "Not connected: " <> show city1 <> " and " <> show city2

twoRandomCitiesAreReachableOrNotButSymmetric :: TwoRandomCities -> Result
twoRandomCitiesAreReachableOrNotButSymmetric (TwoRandomCities (Tuple city1 city2)) =
  assertEq (G.isReachable connections city1 city2) (G.isReachable connections city2 city1)
