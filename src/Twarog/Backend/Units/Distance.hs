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

module Twarog.Backend.Units.Distance
  ( 
  -- * Distance 
    Distance (..)
  -- ** Units
  , Micrometre
  , Milimetre
  , Centimetre
  , Scruple
  , Line
  , Thumb
  , Inch
  , Palm
  , Foot
  , Ell
  , Step
  , Yard
  , Pace
  , Fathom
  , Rod
  , StoneThrow
  , ArrowShoot
  , Furlong
  , Mile
  , Quarter
  , Rest
  , Road
  -- ** Conversions 
  , distance
  , toDistance
  , milimetre
  , centimetre
  , scruple
  , line
  , thumb
  , inch
  , palm
  , foot
  , ell
  , step
  , yard
  , pace
  , fathom
  , rod
  , stoneThrow
  , arrowShoot
  , furlong
  , mile
  , quarter
  , rest
  , road
  ) where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics
import Twarog.Backend.Units.Common

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

instance Semigroup (Distance (dist :: Nat)) where
    (<>) :: Distance dist -> Distance dist -> Distance dist 
    (<>) = coerce ((+) @Natural)
