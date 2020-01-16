{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DerivingStrategies     #-}

module Twarog.Backend.Units.Common
  ( nat 
  )
  where

import Data.Proxy (Proxy (..))
import GHC.Exts (coerce)
import GHC.TypeNats
import GHC.Natural
import GHC.Generics

nat :: forall (nat :: Nat) . KnownNat nat => Natural
nat = natVal (Proxy @nat)