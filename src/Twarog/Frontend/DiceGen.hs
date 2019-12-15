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
  , genAttributes
  , genRace
  , genMonth
  , genBirthday
  )
  where

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
    D6   -> d4
    D8   -> d8
    D10  -> d10
    D12  -> d12
    D20  -> d20
    D100 -> d100

-- | Generate random valid attribute
genAttribute :: Gen Int
genAttribute = do
  x <- roll 3 D6
  y <- roll 3 D6 
  let r = max (sum x) (sum y)
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
genRace = Gen.choice $ Gen.constant <$> races

-- | Generate random month
genMonth :: Gen Month
genMonth = do
  let months' = Gen.constant <$> months
      wMonths = (1, Gen.constant Nyarsdagr) :
        ((27, ) <$> (tail . reverse $ months'))
  Gen.frequency wMonths

-- | Generate random birthday date
genBirthday :: Gen Birthday
genBirthday = do
  m <- genMonth
  if m == Nyarsdagr
  then return $ Birthday NewYearsDay Nyarsdagr
  else do
    cd <- Gen.int $ Range.constant 1 28 
    return $ Birthday (CommonDay cd) m

-- | Generate random archetype
genArchetype :: Gen Archetype
genArchetype = Gen.choice $ Gen.constant <$> archetypes
