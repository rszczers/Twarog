{-# LANGUAGE GADTs #-}
-- | Item datatype for all the game items.
module Twarog.Backend.Item where 

import Twarog.Types
import Twarog.Backend.Units
import Twarog.Backend.Enchantement

-- | Describes how damaged item is.
data DamageDeg = Undamaged -- ^ Item is perfectly fine.
               | Unrepairable -- ^ Item is damaged beyond repair.
               | RepairableInWorkshop  -- ^ PC needs smithy/workshop.
               | Repairable -- ^ PC can repair the item testing Crafts

-- | Describes how well item is made. 
data Quality = Terrible      -- ^ -3 modification
             | Poor          -- ^ -2 modification
             | BelowAvarage  -- ^ -1 modification
             | Avarage 
             | AboveAvarage  -- ^ 1 modification
             | High          -- ^ 2 modification
             | VeryHigh      -- ^ 3 modification
             | Exceptional   -- ^ 4 modification
             | Divine        -- ^ 5 modification
             deriving (Show)

-- | Simplified notions for throwing range.
data MissileRange = ShortRange   -- ^ 1 * 'BaseRange'.
                  | MediumRange  -- ^ 2 * 'BaseRange'; -2 OV Mod, -1 Damage.
                  | LongRange    -- ^ 4 * 'BaseRange'; -6 OV Mod, -2 Damage.
                  | ExtremeRange -- ^ 8 * 'BaseRange'; -12 OV Mod, -3 Damage.
                  deriving (Show)

-- | Melee weapon types
data WeaponType = Spear
                | Sword
                | Dagger
                | NaturalWeapon
                | ConciussionaWeapon
                | Bow
                | Crossbow
                | Sling
                | Staff

-- | Shield types
data ShieldType = Wicker
                | SmallShield
                | MediumShield
                | LargeShield
                | DeeplyDished

-- | Armour types
data ArmourType = LightArmour
                | MediumArmour
                | HeavyArmour

data Other = Tool
           | Special
           | Potion
           | Food
           | Drink
           | Metal
           | Cloth
           | Material

-- | Wrapper for heterogeneous 'Bag'.
data Pack = PackWeapon { unPackWeapon :: Item Weapon }
          | PackArmour { unPackArmour :: Item (Armour BodyArmour) }
          | PackHelmet { unPackHelmet :: Item (Armour Helmet) }
          | PackShield { unPackShield :: Item Shield }
          | PackBag    { unPackBag    :: Item Bag }
          | PackOther  { unPackOther  :: Item Other  }

{- | 
Weapons can be used in melee or thrown; their statistics differ accordingly.
-}
data WeaponAction = Melee { meleeStats :: WeaponStats }
                  | Throw { missileStats :: Distance Foot -> WeaponStats }

-- | Heterogeneous bag.
data Bag = Bag 
  { capacity :: Weight Ounce -- ^ Maximal load.
  , content :: [Pack]
  }

-- | General shield statistics.
data Shield = Shield
  { shieldType    :: ShieldType
  , shieldMe      :: ShieldMe
  , miBlockChance :: MiBlockChance
  , shieldMs      :: ShieldMs
  }

-- | Weapon wrapper.
newtype Weapon = Weapon
  { getStats :: WeaponAction -> WeaponStats }

-- | General weapon damage stats.
data WeaponStats = WeaponStats
  { damage :: Damage -- ^ Phisical damage the weapon deals.
  , cut    :: CutMod -- ^ Cut effects modifier of the weapon.
  , shock  :: ShockMod -- ^ Shock effect modifier of the weapon.
  }

-- | Statistics for lower armour parts. 
data BodyArmour = BodyArmour
  { armourType     :: ArmourType
  , armourMs       :: ArmourMs
  , stealthMod     :: StealthMod
  , swimmingMod    :: SwimmingMod
  , encumbranceMod :: EncumbranceMod
  }

-- | Statistics for helmets.
newtype Helmet = Helmet
  { perceptionMod :: PerceptionMod  -- ^ Wearing helments narrows field of view.
  }

class ArmourPart t
instance ArmourPart BodyArmour
instance ArmourPart Helmet

data Armour t where
  Armour :: ArmourPart t => AV -> t -> Armour t

-- | General item type.
data Item d = Item 
  { details      :: d
  , realValue    :: Value Lead
  , weight       :: Weight Ounce
  , quality      :: Quality
  , baseRange    :: BaseRange
  , name         :: String
  , description  :: String
  , damageDegree :: DamageDeg
  , enchantments :: [Enchantement]
  }
