module Twarog.Backend.Encumbrance
  (
  -- * Encumbrance
    Encumbrance (..)
  , EncumbranceMod
  -- ** Utils
  , encumbrance
  , strEncumbranceMod
  )
  where

import Twarog.Backend.Types
import Twarog.Backend.Flaws

type EncumbranceMod = Float

data Encumbrance = LightLoad    -- ^ '0' MS mod
                 | MediumLoad   -- ^ '-1' MS mod
                 | HeavyLoad    -- ^ '-2' MS mod
                 | AbsurdLoad
                 deriving (Show)

-- | Converts encumbrance factor to apriopriate notion of @Encumbrance@
encumbrance :: Float -> Encumbrance
encumbrance x | x <= 1          = LightLoad
              | 1 < x && x <= 2 = MediumLoad
              | 2 < x && x <= 3 = HeavyLoad
              | x > 3           = AbsurdLoad -- PC can only lift sth or push it

strEncumbranceMod :: Str -> EncumbranceMod -> EncumbranceMod
strEncumbranceMod str = \x -> x - (-0.25 * (fromIntegral $ str 0))
 
