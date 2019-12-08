module Twarog.Backend.Types
  ( 
  -- * Character sheet
  -- ** Descriptive part
    Race 
  , CharacterRole
  , Sex
  , Height
  , Size
  , LifeStance
  , Age
  , Attitude
  , Archetype
  , XP
  , Lvl
  , Hamingja
  -- ** Attributes
  , CHA
  , CON
  , DEX
  , STR
  , WIL
  , INT
  , Attributes (..)
  -- ** Modifiers
  , Cha
  , Con
  , Dex
  , Str
  , Wil
  , Modifiers (..)
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
  -- ** Toughness
  , Cold
  , Electricity
  , Heat
  , Physical
  , Toughness
  -- ** Condition
  , Condition
  -- ** Encumbrance
  , Encumbrance
  -- ** Vitality
  , HP
  , SP
  -- ** Resistance
  , Resistance
  , Disease
  , Poison
  -- ** Morale
  , Morale
  -- * Item statistics
  -- ** Armour statistics
  , AV 
  , ArmourMs
  , StealthMod
  , SwimmingMod
  , EncumbranceMod
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

import Twarog.Backend.Archetypes (Attitude(..), Archetype(..))

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
type ShieldBlock = Int 

type Hamingja = Int
type BaseRange = Int

type ArmourMs = Int
type StealthMod = Int
type SwimmingMod = Int   
type EncumbranceMod = Int 
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
type Age = Int
type Height = Int
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

data Race = Dwarf
          | Elf
          | Hobgoblin
          | HalfOrc
          | Goblin
          | Ogre
          | CommonOrc
          | Gnome
          | Halfling
          | CommonMan
          | LesserMan
          | HighMan
          deriving (Show)

data CharacterRole = Civilian
                   | Warrior
                   | Stalker
                   | Trickster
                   | Ranger
                   | Bard
                   | Sorcerer
                   | DwarfRole
                   | ElfRole
                   | GnomeRole
                   | HalflingRole
                   | OrcRole
                   deriving (Show)

data Sex = Male
         | Female
         deriving (Show)

data LifeStance = Religious
                | Traditional
                deriving (Show)

data Toughness = Toughness Cold Electricity Heat Physical
  deriving (Show)

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

data Encumbrance = LightLoad    -- ^ '0' MS mod
                 | MediumLoad   -- ^ '-1' MS mod
                 | HeavyLoad    -- ^ '-2' MS mod
                 deriving (Show)

data Resistance = Resistance
  { disease :: Disease
  , poison  :: Poison
  } deriving (Show)

data Attributes = Attributes
  { cha :: CHA
  , con :: CON
  , dex :: DEX
  , int :: INT
  , str :: STR
  , wil :: WIL
  } deriving (Show)
              
data Modifiers = Modifiers
  { chaMod :: Cha
  , conMod :: Con
  , dexMod :: Dex
  , intMod :: Int -> Int
  , strMod :: Str
  , wilMod :: Wil
  } 
