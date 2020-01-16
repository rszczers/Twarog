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

module Twarog.Backend.Units.Weight
  (
  -- * Weight
  -- ** Units
    Weight (..)
  , Miligram
  , Gram
  , Kilogram
  , Carat
  , Ort
  , Sicilium
  , Ounce 
  , Mark
  , BowlPound
  , WeightPound
  -- ** Conversions 
  , gram
  , kilogram
  , carat
  , ort
  , sicilium
  , ounce 
  , mark
  , bowlPound
  , weightPound
  ) where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics
import Twarog.Backend.Units.Common

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

weightPound :: Natural -> Weight Ounce
weightPound = weight

toWeight :: forall (to :: Nat) (from :: Nat) . Weight from -> Weight to
toWeight = coerce