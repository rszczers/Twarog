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
  -- ** Attribute generator
  , genAttribute
  )
  where

import Twarog.Backend.Types
import Twarog.Backend.Races

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
roll n = \case
  D4   -> Gen.list (Range.singleton n) d4
  D6   -> Gen.list (Range.singleton n) d4
  D8   -> Gen.list (Range.singleton n) d8
  D10  -> Gen.list (Range.singleton n) d10
  D12  -> Gen.list (Range.singleton n) d12
  D20  -> Gen.list (Range.singleton n) d20
  D100 -> Gen.list (Range.singleton n) d100

-- | Generate random valid attribute
genAttribute :: Gen Int
genAttribute = do
  x <- d20
  y <- d20
  let r = max x y
  if r > 3
  then return r
  else genAttribute

-- | Generate random attributes
genAttributes :: Gen Attributes
genAttributes = do
  cha <- genAttribute
  con <- genAttribute
  dex <- genAttribute
  int <- genAttribute
  str <- genAttribute
  wil <- genAttribute
  return $ Attributes cha con dex int str wil

-- | Generate random race
genRace :: Gen Race
genRace = Gen.choice $ Gen.constant <$> race 
