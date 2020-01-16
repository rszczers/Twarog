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

module Twarog.Backend.Units.Value 
  (
  -- * Value
    Value
  -- ** Units
  , Lead
  , Iron
  , Copper
  , Salt
  , Bronze
  , Silver
  , Aurichalcum
  , Electrum
  , Gold
  -- ** Conversions
  , lead
  , iron
  , copper
  , salt
  , bronze
  , silver
  , aurichalcum
  , electrum
  , gold
  ) where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics
import Twarog.Backend.Units.Common

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
