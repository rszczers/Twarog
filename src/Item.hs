module Item where 

import Types
import Units
import Enchantement

data DamageDeg = Undamaged
               | Unrepairable
               | RepairableInWorkshop
               | Repairable

data Quality = Terrible 
             | Poor 
             | BelowAvarage 
             | Avarage
             | AboveAvarage
             | High
             | Exceptional
             | Divine
             deriving (Show)

data MissileRange = ShortRange
                  | MediumRange
                  | LongRange
                  | ExtremeRange
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
           | Metal
           | Cloth
           | Material

data Pack = PackWeapon { unPackWeapon :: Item Weapon }
          | PackArmour { unPackArmour :: Item (Armour BodyArmour) }
          | PackHelmet { unPackHelmet :: Item (Armour Helmet) }
          | PackShield { unPackShield :: Item Shield }
          | PackBag    { unPackBag    :: Item Bag }
          | PackOther  { unPackOther  :: Item Other  }

data WeaponAction = Melee { meleeStats :: WeaponStats }
                  | Throw { missileStats :: Distance Foot -> WeaponStats }

data Bag = Bag 
  { capacity :: Weight Ounce
  , content :: [Pack]
  }

data Shield = Shield
  { shieldType    :: ShieldType
  , shieldMe      :: ShieldMe
  , miBlockChance :: MiBlockChance
  , shieldMs      :: ShieldMs
  }

newtype Weapon = Weapon
  { getStats :: WeaponAction -> WeaponStats }

data WeaponStats = WeaponStats
  { damage :: Damage
  , cut    :: CutMod
  , shock  :: ShockMod
  }

data BodyArmour = BodyArmour
  { armourType     :: ArmourType
  , armourMs       :: ArmourMs
  , stealthMod     :: StealthMod
  , swimmingMod    :: SwimmingMod
  , encumbranceMod :: EncumbranceMod
  }

newtype Helmet = Helmet
  { perceptionMod :: PerceptionMod }

data Armour t = Armour
  { av :: Int
  , armour :: t }

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
