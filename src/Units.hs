{- |
This module is inspired in may ways by
https://kowainik.github.io/posts/membrain
-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DerivingStrategies     #-}

#if ( __GLASGOW_HASKELL__ >= 806 )
{-# LANGUAGE NoStarIsType #-}
#endif

module Units where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics

-- | Weight units
type Miligram = 1
type Gram = 1000 * Miligram
type Kilogram = 1000 * Gram
type Carat = 200 * Miligram
type Ort = 5 * Carat
type Sicilium = 4 * Ort
type Ounce  = 4 * Sicilium
type Mark = 16 * Ounce
type BowlPound = 2 * Mark
type WeightPound = 12 * BowlPound

newtype Weight (wgh :: Nat) = Weight
    { unWeight :: Natural
    } deriving stock   (Show, Read, Generic)
      deriving newtype (Eq, Ord)

nat :: forall (nat :: Nat) . KnownNat nat => Natural
nat = natVal (Proxy @nat)

weight :: forall (wgh :: Nat) . KnownNat wgh => Natural -> Weight wgh
weight = Weight . (* nat @wgh)

gram :: Natural -> Weight Gram
gram = weight

kilogram :: Natural -> Weight Kilogram
kilogram = weight

carat :: Natural -> Weight Carat
carat = weight

ort :: Natural -> Weight Ort
ort = weight

sicilium :: Natural -> Weight Sicilium
sicilium = weight

ounce :: Natural -> Weight Ounce
ounce = weight

mark :: Natural -> Weight Mark
mark = weight

bowlPound :: Natural -> Weight Ounce
bowlPound = weight

weightPount :: Natural -> Weight Ounce
weightPount = weight

toWeight :: forall (to :: Nat) (from :: Nat) . Weight from -> Weight to
toWeight = coerce

-- | Distance units
type Micrometre = 1
type Milimetre = 1000 * Micrometre
type Centimetre = 10 * Milimetre
type Scruple = 1800 * Micrometre
type Line = 2 * Milimetre
type Thumb = 18500 * Milimetre
type Inch = 24600 * Milimetre
type Palm = 3 * Thumb
type Foot = 4 * Palm
type Ell = 2 * Foot
type Step = 74 * Centimetre
type Yard = 3 * Foot
type Pace = 2 * Step
type Fathom = 3 * Ell
type Rod = 5 * Ell
type StoneThrow = 60 * Ell
type ArrowShoot = 4 * StoneThrow
type Furlong = 625 * Foot
type Mile = 1000 * Pace
type Quarter = 9000 * Foot
type Rest = 4 * Quarter
type Road = 4 * Rest

milimetre :: Natural -> Distance Milimetre
milimetre = distance

centimetre :: Natural -> Distance Centimetre
centimetre = distance

scruple :: Natural -> Distance Scruple
scruple = distance

line :: Natural -> Distance Line
line = distance

thumb :: Natural -> Distance Thumb
thumb = distance

inch :: Natural -> Distance Inch
inch = distance

palm :: Natural -> Distance Palm
palm = distance

foot :: Natural -> Distance Foot
foot = distance

ell :: Natural -> Distance Ell
ell = distance

step :: Natural -> Distance Step
step = distance

yard :: Natural -> Distance Yard
yard = distance

pace :: Natural -> Distance Pace
pace = distance

fathom :: Natural -> Distance Fathom
fathom = distance

rod :: Natural -> Distance Rod
rod = distance

stoneThrow :: Natural -> Distance StoneThrow
stoneThrow = distance

arrowShoot :: Natural -> Distance ArrowShoot
arrowShoot = distance

furlong :: Natural -> Distance Furlong
furlong = distance

mile :: Natural -> Distance Mile
mile = distance

quarter :: Natural -> Distance Quarter
quarter = distance

rest :: Natural -> Distance Rest
rest = distance

road :: Natural -> Distance Road
road = distance

newtype Distance (wgh :: Nat) = Distance
    { unDistance :: Natural
    } deriving stock   (Show, Read, Generic)
      deriving newtype (Eq, Ord)

distance :: forall (dist :: Nat) . KnownNat dist => Natural -> Distance dist 
distance = Distance . (* nat @dist)

toDistance :: forall (to :: Nat) (from :: Nat) . Distance from -> Distance to
toDistance = coerce

-- | Capacity units
type Microlitre = 1
type Dose = 62500 * Microlitre
type Pel = 225000 * Microlitre
type Just = 3 * Pel
type Pot = 4 * Pel
type Can = 32 * Dose
type Bowl = 3 * Pot
type Barrel = 60 * Can
type Cargo = 12 * Barrel

microlitre :: Natural -> Capacity Microlitre
microlitre = capacity

dose :: Natural -> Capacity Dose
dose = capacity

pel :: Natural -> Capacity Pel
pel = capacity

just :: Natural -> Capacity Just
just = capacity

pot :: Natural -> Capacity Pot
pot = capacity

can :: Natural -> Capacity Can
can = capacity

bowl :: Natural -> Capacity Bowl
bowl = capacity

barrel :: Natural -> Capacity Barrel
barrel = capacity

cargo :: Natural -> Capacity Cargo
cargo = capacity

newtype Capacity (wgh :: Nat) = Capacity
    { unCapacity :: Natural
    } deriving stock   (Show, Read, Generic)
      deriving newtype (Eq, Ord)

capacity :: forall (cap :: Nat) . KnownNat cap => Natural -> Capacity cap 
capacity = Capacity . (* nat @cap)

toCapacity :: forall (to :: Nat) (from :: Nat) . Capacity from -> Capacity to
toCapacity = coerce

-- | Time
type Glass = 1
type Hour = 2 * Glass
type Shift = 4 * Glass
type Day = 6 * Shift

glass :: Natural -> Time Glass
glass = time

hour :: Natural -> Time Hour
hour = time

shift :: Natural -> Time Shift
shift = time

day :: Natural -> Time Day
day = time

newtype Time (t :: Nat) = Time
    { unTime :: Natural
    } deriving stock   (Show, Read, Generic)
      deriving newtype (Eq, Ord)

time :: forall (t :: Nat) . KnownNat t => Natural -> Time t 
time = Time . (* nat @t)

toTime :: forall (to :: Nat) (from :: Nat) . Time from -> Time to
toTime = coerce

-- | Value
type Lead = 1
type Iron = 2 * Lead
type Copper = 2 * Iron
type Salt = 1 * Copper
type Bronze = 2 * Copper
type Silver = 30 * Bronze
type Aurichalcum = 10 * Silver
type Electrum = 15 * Silver
type Gold = 2 * Aurichalcum

lead :: Natural -> Value Lead
lead = value

iron :: Natural -> Value Iron
iron = value

copper :: Natural -> Value Copper
copper = value

salt :: Natural -> Value Salt
salt = value

bronze :: Natural -> Value Bronze
bronze = value

silver :: Natural -> Value Silver
silver = value

aurichalcum :: Natural -> Value Aurichalcum
aurichalcum = value

electrum :: Natural -> Value Electrum
electrum = value

gold :: Natural -> Value Gold
gold = value

newtype Value (t :: Nat) = Value
    { unValue :: Natural
    } deriving stock   (Show, Read, Generic)
      deriving newtype (Eq, Ord)

value :: forall (t :: Nat) . KnownNat t => Natural -> Value t 
value = Value . (* nat @t)

toValue :: forall (to :: Nat) (from :: Nat) . Value from -> Value to
toValue = coerce
