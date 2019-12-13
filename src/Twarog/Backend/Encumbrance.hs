module Twarog.Backend.Encumbrance
  (
  -- * Encumbrance
    Encumbrance
  , EncumbranceMod
  -- ** Utils
  , encumbrance
  , encumbranceMod
  )
  where

import Twarog.Backend.Types
import Twarog.Backend.Flaws

type EncumbranceMod = Float -> Float

instance Show EncumbranceMod where
  show f = show $ f 0.0

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
              | x > 3           = AbsurdLoad -- ^ PC can only lift sth or push it

-- | Gets list of flaws caused by encumbrance
encumbranceMod :: Float -> [FlawMod]
encumbranceMod x
  | x <= -1.5              = [ StrFlaw (+ 6) ]
  | -1.5 < x && x <= -1.25 = [ StrFlaw (+ 5) ]
  | -1.25 < x && x <= -1.0 = [ StrFlaw (+ 4) ]
  | -1.0 < x && x <= -0.75 = [ StrFlaw (+ 3) ]
  | -0.75 < x && x <= -0.5 = [ StrFlaw (+ 2) ]
  | -0.5 < x && x <= -0.25 = [ StrFlaw (+ 1) ]
  | -0.25 < x && x <= 0.25 = [ StrFlaw (+ 0) ]
  | 0.25 < x && x <= 0.5   = [ StrFlaw (\x -> x - 1) ]
  | 0.5 < x && x <= 0.75   = [ StrFlaw (\x -> x - 2) ]
                             ++ (flawMod $ BadBack FlawLevel1)
  | 0.75 < x && x <= 1.0   = [ StrFlaw (\x -> x - 3) ]
                             ++ (flawMod $ BadBack FlawLevel2)
  | 1.0 < x && x <= 1.25   = [ StrFlaw (\x -> x - 4) ]
                             ++ (flawMod $ BadBack FlawLevel3)
  | 1.25 < x               = [ StrFlaw (\x -> x - 5) ]
                             ++ (flawMod $ BadBack FlawLevel3)
