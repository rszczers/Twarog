module Twarog.Frontend.DiceGen
  (
  -- * Generators
  -- ** Dice roll generators
    d4
  , d6
  , d8
  , d10
  , d12
  , d20
  , d100
  , roll
  ) where

import Twarog.Backend.Types
import Twarog.Backend.Races
import Twarog.Backend.Calendar
import Twarog.Backend.Archetypes
import Twarog.Backend.Gods

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

d4 :: Gen Int
d4 = Gen.int $ Range.constant 1 4

d6 :: Gen Int
d6 = Gen.int $ Range.constant 1 6

d8 :: Gen Int
d8 = Gen.int $ Range.constant 1 8

d10 :: Gen Int
d10 = Gen.int $ Range.constant 1 10

d12 :: Gen Int
d12 = Gen.int $ Range.constant 1 12

d20 :: Gen Int
d20 = Gen.int $ Range.constant 1 20

d100 :: Gen Int
d100 = Gen.int $ Range.constant 1 100

-- | Roll n-times given dice
roll :: Int -> Dice -> Gen [Int]
roll n dice = Gen.list (Range.singleton n) $
  case dice of
    D4   -> d4
    D6   -> d6
    D8   -> d8
    D10  -> d10
    D12  -> d12
    D20  -> d20
    D100 -> d100
