{-# LANGUAGE GADTs #-}
-- | Item datatype for all the game items.
module Twarog.Backend.Item 
  ( Item (..)
  , Bag  
  , Armour
  , BodyArmour
  , Helmet
  , Shield
  , Weapon
  )
  where 

import Twarog.Backend.Types
import Twarog.Backend.Units
import Twarog.Backend.Enchantement

-- | Describes how damaged item is.
data DamageDeg = Undamaged             -- ^ Item is perfectly fine.
               | Unrepairablei         -- ^ Item is damaged beyond repair.
               | RepairableInWorkshop  -- ^ PC needs smithy/workshop.
               | Repairable         -- ^ PC can repair the item testing Crafts
               deriving (Show)

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
                  | MediumRange  -- ^ 2 * 'BaseRange'; '-2' OV Mod, -1 Damage.
                  | LongRange    -- ^ 4 * 'BaseRange'; '-6' OV Mod, -2 Damage.
                  | ExtremeRange -- ^ 8 * 'BaseRange'; '-12' OV Mod, -3 Damage.
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
                deriving (Show)

-- | Shield types
data ShieldType = Wicker
                | SmallShield
                | MediumShield
                | LargeShield
                | DeeplyDished
                deriving (Show)

-- | Armour types
data ArmourType = LightArmour
                | MediumArmour
                | HeavyArmour
                deriving (Show)

data Other = Tool
           | Special
           | Food
           | Drink
           | Herb
           | Metal
           | Cloth
           | Material
           deriving (Show)

-- | Wrapper for heterogeneous 'Bag'.
data Pack = PackWeapon (Item Weapon) 
          | PackArmour (Item (Armour BodyArmour)) 
          | PackHelmet (Item (Armour Helmet)) 
          | PackShield (Item Shield) 
          | PackBag    (Item Bag) 
          | PackOther  (Item Other)
          deriving (Show)

{- | 
Weapons can be used in melee or thrown; their statistics differ accordingly.
-}
newtype MeleeStats = Melee { unMelee :: WeaponStats }
  deriving (Show)

newtype MissileStats = Throw { unThrow :: Distance Foot -> WeaponStats }
 
instance Show MissileStats where
  show (Throw f) = show . f $ foot 0 

-- | Heterogeneous bag.
data Bag = Bag 
  { capacity :: Weight Ounce -- ^ Maximal load.
  , content :: [Pack]
  } deriving (Show)

-- | General shield statistics.
data Shield = Shield
  { shieldType    :: ShieldType
  , shieldMe      :: ShieldMe
  , miBlockChance :: MiBlockChance
  , shieldMs      :: ShieldMs
  } deriving (Show)

-- | Weapon wrapper.
data Weapon = Weapon
  { meleeStats   :: MeleeStats
  , missileStats :: MissileStats
  } deriving (Show)

-- | General weapon damage stats.
data WeaponStats = WeaponStats
  { damage :: Damage -- ^ Phisical damage the weapon deals.
  , cut    :: CutMod -- ^ Cut effects modifier of the weapon.
  , shock  :: ShockMod -- ^ Shock effect modifier of the weapon.
  } deriving (Show)

-- | Statistics for lower armour parts. 
data BodyArmour = BodyArmour
  { armourType     :: ArmourType
  , armourMs       :: ArmourMs
  , stealthMod     :: StealthMod
  , swimmingMod    :: SwimmingMod
  , encumbranceMod :: EncumbranceMod
  } deriving (Show)

-- | Statistics for helmets.
newtype Helmet = Helmet
  { perceptionMod :: PerceptionMod  -- ^ Wearing helments narrows field of view.
  } deriving (Show)

class (Show t) => ArmourPart t
instance ArmourPart BodyArmour
instance ArmourPart Helmet

data Armour t where
  Armour :: ArmourPart t => AV -> t -> Armour t

instance Show (Armour t) where
  show (Armour av t) = "Armour" ++ show av ++ " " ++ show t

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
  } deriving (Show)
