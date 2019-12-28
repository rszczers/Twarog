module Twarog.Backend.Types
  (
  -- * Dices
    Dice (..)
  -- * Character sheet
  -- ** Sheet owner
  , Owner (..)
  , CharacterName
  , Note
  -- ** Descriptive part
  , Name
  , Sex (..)
  , sexes
  , Height
  , Size (..)
  , Age (..)
  , _Immortal
  , _Mortal
  , XP
  , Lvl
  , Hamingja
  , Mod
  -- ** Attributes
  , CHA
  , CON
  , DEX
  , STR
  , WIL
  , INT
  , Attributes (..)
  -- *** Attributes lenses
  , cha
  , con
  , dex
  , int
  , str
  , wil
  -- ** Modifiers
  , Cha
  , Con
  , Dex
  , Str
  , Wil
  , Modifiers (..)
  , toModifier
  , modifiers
  -- *** Modifiers lenses
  , chaMod
  , conMod
  , dexMod
  , intMod
  , strMod
  , wilMod
  -- ** Combat statistics
  , OvMe
  , OvMi
  , DvMe
  , DvMi
  , Dodging
  , TotalAv
  , MsPenality
  , ShieldDvMe
  , ShieldBlock
  , Fright
  -- ** Toughness
  , Cold
  , Electricity
  , Heat
  , Physical
  , Toughness (..)
  , emptyToughness
  -- ** Toughness lenses
  , toughnessCold
  , toughnessElectricity
  , toughnessHeat
  , toughnessPhysical
  -- ** Condition
  , Condition
  -- ** Vitality
  , HP
  , SP
  -- ** Resistance
  , Resistance (..)
  , emptyResistance
  , Disease
  , Poison
  -- *** Resistance lenses
  , disease
  , poison
  -- ** Morale
  , Morale
  -- * Item statistics
  -- ** Armour statistics
  , AV
  , ArmourMs
  , StealthMod
  , SwimmingMod
  , PerceptionMod -- For helmets only
  -- ** Shield statistics
  , ShieldMe
  , MiBlockChance
  , ShieldMs
  -- ** Weapon statistics
  , Damage
  , CutMod
  , ShockMod
  , BaseRange -- Bows and crossbows only
  ) where

import Data.Text
import Control.Lens
import Twarog.Backend.Units

data Dice = D4
          | D6
          | D8
          | D10
          | D12
          | D20
          | D100
          deriving (Show, Eq)

type Name = Text
type Owner = Text
type CharacterName = Text
type Note = Text

type AV = Int
type Damage = Int
type OvMe = Int
type OvMi = Int
type DvMe = Int
type DvMi = Int
type Dodging = Int
type TotalAv = Int
type MsPenality = Int
type ShieldDvMe = Int
type ShieldBlock = Float

type Fright = Int

type Mod = Int

type Hamingja = Int
type BaseRange = Int

type ArmourMs = Int
type StealthMod = Int
type SwimmingMod = Int

type PerceptionMod = Int

type ShieldMe = Int
type MiBlockChance = Int
type ShieldMs = Int

type CutMod = Int
type ShockMod = Int

-- | Resistance
type Disease = Int
type Poison = Int

-- | Toughness
type Cold = Int
type Electricity = Int
type Heat = Int
type Physical = Int

type XP = Int
type HP = Int
type SP = Int

data Age = Immortal | Mortal Int
  deriving (Show)
makePrisms ''Age

type Height = Distance Inch

type Size = Int

type Cha = Int -> Int
type Con = Int -> Int
type Dex = Int -> Int
type Str = Int -> Int
type Wil = Int -> Int

type CHA = Int
type CON = Int
type DEX = Int
type STR = Int
type WIL = Int
type INT = Int

type Lvl = Int


data Sex = Male
         | Female
         | Non
         deriving (Eq, Enum)

instance Show Sex where
  show Male = "Male"
  show Female = "Female"
  show Non = "Nonbinary"

sexes :: [Sex]
sexes = enumFrom (toEnum 0)

data Toughness = Toughness
  { _toughnessCold :: Cold
  , _toughnessElectricity :: Electricity
  , _toughnessHeat :: Heat
  , _toughnessPhysical :: Physical
  } deriving (Show)
makeLenses ''Toughness

emptyToughness :: Toughness
emptyToughness =
  let _toughnessCold = 0
      _toughnessElectricity = 0
      _toughnessHeat = 0
      _toughnessPhysical = 0
   in Toughness{..}

data Condition = Tired
               | Weary
               | Exhausted
               | Wet
               | SoakingWet
               deriving (Show)

data Morale = Nervous
            | Afraid
            | Anxious
            | Terrified
            | Panic
            deriving (Show)

data Resistance = Resistance
  { _disease :: Disease
  , _poison  :: Poison
  } deriving (Show)
makeLenses ''Resistance

emptyResistance :: Resistance
emptyResistance =
  let _disease = 0
      _poison = 0
   in Resistance{..}

data Attributes = Attributes
  { _cha :: CHA
  , _con :: CON
  , _dex :: DEX
  , _int :: INT
  , _str :: STR
  , _wil :: WIL
  } deriving (Eq, Show)
makeLenses ''Attributes

data Modifiers = Modifiers
  { _chaMod :: Cha
  , _conMod :: Con
  , _dexMod :: Dex
  , _intMod :: Int -> Int
  , _strMod :: Str
  , _wilMod :: Wil
  }
makeLenses ''Modifiers

instance Show Modifiers where
  show (Modifiers a b c d e f) =
    let xs = [a, b, c, d, e, f] <*> [0]
     in show xs

toModifier :: Int -> Int -> Int
toModifier x  | x <= 1             = \x -> x - 5
              | x == 2             = \x -> x - 4
              | x == 3             = \x -> x - 3
              | x == 4 || x == 5   = \x -> x - 2
              | 6 <= x && x <= 8   = \x -> x - 1
              | 9 <= x && x <= 12  = id
              | 13 <= x && x <= 15 = (+ 1)
              | 16 <= x && x <= 17 = (+ 2)
              | 18 <= x && x <= 19 = (+ 3)
              | x == 20            = (+ 4)
              | x == 21            = (+ 5)
              | x >= 22            = (+ 6)

-- | Convert Attributes to Modifiers
modifiers :: Attributes -> Modifiers
modifiers (Attributes cha con dex int str wil) =
  let f = toModifier
   in Modifiers (f cha) (f con) (f dex) (f int) (f str) (f wil)

