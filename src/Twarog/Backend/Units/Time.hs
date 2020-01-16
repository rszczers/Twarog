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

module Twarog.Backend.Units.Time 
  (
  -- * Time
    Time
  -- ** Units
  , Glass
  , Hour
  , Shift
  -- ** Conversions
  , glass
  , hour
  , shift
  , day
  ) where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics
import Twarog.Backend.Units.Common

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
