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

module Twarog.Backend.Units.Capacity 
  (
  -- * Capacity
    Capacity
  -- ** Units
  , Microlitre
  , Dose
  , Pel
  , Just
  , Pot
  , Can
  , Bowl
  , Barrel
  , Cargo
  -- ** Conversions
  , microlitre
  , dose
  , pel
  , just
  , pot
  , can
  , bowl
  , barrel
  , cargo
  ) where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics
import Twarog.Backend.Units.Common

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
