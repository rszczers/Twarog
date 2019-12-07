{-# LANGUAGE TemplateHaskell #-}

module Twarog.Backend.Character where

import Data.Set
import Control.Lens

import Twarog.Types
import Twarog.Backend.Archetypes
import Twarog.Backend.Item

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

data Skill = Acrobatics
           | Acting
           | Alchemy
           | Climbing
           | Crafts
           | Dancing
           | Dodging
           | FlutePlaying
           | Foraging
           | Fortitude
           | Healing
           | LyrePlaying
           | Mechanics
           | Melee
           | Missile
           | Navigation
           | Perception
           | Poetry
           | ReligiousTradition
           | Riding
           | RuneLore
           | Seamanship
           | Singing
           | SocialSkills
           | Stamina
           | Stealth
           | Swimming
           | Tempo
           | Tracking
           | Trickery
           | WorldLore
           deriving (Eq, Ord, Show)

data Proficiency = Trained
                 | Untrained
                 | CharacterRoleSkill
                 deriving (Eq, Ord, Show)

data SkillType = MovementSkill
               | SpecialSkill
               | CombatSkill
               deriving (Eq, Ord, Show)

data CharacterSkill = CharacterSkill
  { characterSkill :: Skill
  , skillType      :: SkillType
  , proficiency    :: Proficiency
  } deriving (Eq, Ord, Show)

class Applicable t
instance Applicable Morale
instance Applicable Encumbrance
instance Applicable Condition
instance Applicable CharacterSkill
instance Applicable Race

-- -- | Apply arbitrary modifier to given character 
-- applyMod :: Applicable t => t -> Character -> Character
-- applyMod Acrobatic
-- applyMod Aggresive
-- applyMod AnimalFriend
-- applyMod Arachnean
-- applyMod Argonautic
-- applyMod Ascetic
-- applyMod Athletic
-- applyMod Bloodhound
-- applyMod Calliopean
-- applyMod Careful
-- applyMod Caliopean
-- applyMod Courageous
-- applyMod Craftsman
-- applyMod Curious
-- applyMod DartThrower
-- applyMod DeepBreather
-- applyMod Dodger
-- applyMod Durable
-- applyMod Empathic
-- applyMod Enduring
-- applyMod Eratorean
-- applyMod Euterpean
-- applyMod Fast
-- applyMod FastSleeper
-- applyMod Favourite
-- applyMod Fearless
-- applyMod Fighter
-- applyMod FistFighter
-- applyMod Focused
-- applyMod GoodReflexes
-- applyMod HawkEyed
-- applyMod Hephaestusean
-- applyMod Heraklean
-- applyMod Herbalist
-- applyMod Humble
-- applyMod Inquisitive
-- applyMod Lancer
-- applyMod LightFooted
-- applyMod LynxEyes
-- applyMod Mariner
-- applyMod Marked
-- applyMod Mechanic
-- applyMod Malpogomeanean
-- applyMod Mermaid
-- applyMod Mule
-- applyMod Nimble
-- applyMod Perseusean
-- applyMod Pietistic
-- applyMod Polyhymnian
-- applyMod Rider
-- applyMod Sensitive
-- applyMod Sharpshooter
-- applyMod Shooter
-- applyMod Sirenean
-- applyMod Slinger
-- applyMod SlowAgeing
-- applyMod SpearThrower
-- applyMod Springy
-- applyMod StrongBack
-- applyMod StrongGrip
-- applyMod Survivor
-- applyMod Swimmer
-- applyMod SwordDancer
-- applyMod Terpsichorean
-- applyMod Thalian
-- applyMod Thrower
-- applyMod Tough
-- applyMod Tracker
-- applyMod TricksterTalent
-- applyMod Uranian
-- applyMod WarmHands
-- applyMod Zevsean
-- applyMod Aegirean

data Toughness = Toughness Cold Electricity Heat Physical
  deriving (Show)

{- |
Character sheet, non-specific to combat mode, i.e. we 
don't consider e.g. Morale.
-}

data Equipment = Equipment
  { belt          :: Maybe Bag
  , pouch         :: Maybe Bag
  , quiver        :: Maybe Bag
  , leftShoulder  :: Maybe Bag
  , rightShoulder :: Maybe Bag
  , sack          :: Maybe Bag
  , backpack      :: Maybe Bag
  , armour        :: Maybe (Item (Armour BodyArmour))
  , helmet        :: Maybe (Item (Armour Helmet))
  , shield        :: Maybe (Item Shield)
  , meleeWeapon   :: Maybe (Item Weapon)
  , missileWeapon :: Maybe (Item Weapon)
  , clothes       :: Maybe Bag
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

instance Show Modifiers where
  show (Modifiers a b c d e f) =
    let xs = [a, b, c, d, e, f] <*> [0]
     in show xs

modifiers :: Attributes -> Modifiers 
modifiers (Attributes cha con dex int str wil) =
  let xs = [cha, con, dex, int, str, wil]
      f x | x <= 1             = \x -> x - 5
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
      [cha', con', dex', int', str', wil'] = f <$> xs        
  in Modifiers cha' con' dex' int' str' wil'      

data CombatStats = CombatStats
  { ovMe        :: OvMe
  , ovMi        :: OvMi
  , dvMe        :: DvMe
  , dvMi        :: DvMi
  , dodging     :: Dodging
  , totalAv     :: TotalAv
  , msPenality  :: MsPenality
  , shieldDvMe  :: ShieldDvMe
  , shieldBlock :: ShieldBlock
  } deriving (Show)

data Resistance = Resistance
  { disease :: Disease
  , poison  :: Poison
  } deriving (Show)

data Talent = Acrobatic
            | Aggresive
            | AnimalFriend
            | Arachnean
            | Argonautic
            | Ascetic
            | Athletic
            | Bloodhound
            | Calliopean
            | Careful
            | Caliopean
            | Courageous
            | Craftsman
            | Curious
            | DartThrower
            | DeepBreather
            | Dodger
            | Durable
            | Empathic
            | Enduring
            | Eratorean
            | Euterpean
            | Fast
            | FastSleeper
            | Favourite
            | Fearless
            | Fighter
            | FistFighter
            | Focused
            | GoodReflexes
            | HawkEyed
            | Hephaestusean
            | Heraklean
            | Herbalist
            | Humble
            | Inquisitive
            | Lancer
            | LightFooted
            | LynxEyes
            | Mariner
            | Marked
            | Mechanic
            | Malpogomeanean
            | Mermaid
            | Mule
            | Nimble
            | Perseusean
            | Pietistic
            | Polyhymnian
            | Rider
            | Sensitive
            | Sharpshooter
            | Shooter
            | Sirenean
            | Slinger
            | SlowAgeing
            | SpearThrower
            | Springy
            | StrongBack
            | StrongGrip
            | Survivor
            | Swimmer
            | SwordDancer
            | Terpsichorean
            | Thalian
            | Thrower
            | Tough
            | Tracker
            | TricksterTalent
            | Uranian
            | WarmHands
            | Zevsean
            | Aegirean
            deriving (Show)

data Flaw = Alcoholic Int
          | Annoying Int
          | BadBack Int
          | BadSight Int
          | BadTempered Int
          | ChronicPain Int
          | Clumsy Int
          | Coward Int
          | Delusional Int
          | Depressed Int
          | Dislike Int
          | Dyslexia Int
          | Enemy Int
          | Fearful Int
          | Frail Int
          | Gluttonous Int
          | Greedy Int
          | Gullible Int
          | Haemophilic Int
          | Hypersexual Int
          | Jealous Int
          | Lawful Int
          | Lazy Int
          | Limp Int
          | LowSelfEsteem Int
          | Seasickness Int
          | OverConfident Int
          | Paranoid Int
          | Parasite Int
          | Philia Int
          | Phobia Int
          | PhysicalDefect Int
          | PhysicalWeakness Int
          | PoorHearing Int
          | Secret Int
          | SelfHating Int
          | Selfish Int
          | Selfless Int
          | Sickly Int
          | ShortLived Int
          | Shy Int
          | SlaveMinded Int
          | Stubborn Int
          | Stuttering Int
          | Unlucky Int
          | Vulnerable Int
          | WeakMinded Int
          | Whiny Int
          deriving (Show)

data Character = Player
  { _playersName :: String
  , _charName    :: String
  , _level       :: Lvl
  , _role        :: CharacterRole
  , _age         :: Age
  , _race        :: Race
  , _height      :: Height
  , _size        :: Size
  , _lifeStance  :: LifeStance
  , _alignment   :: Archetype
  , _stamina     :: SP
  , _health      :: HP
  , _toughness   :: Toughness
  , _experience  :: XP
  , _resistance  :: Resistance
  , _flaws       :: [Flaw]
  , _talents     :: [Talent]
  , _skills      :: Set CharacterSkill
  , _equipment   :: Equipment
  , _attributes  :: Attributes
  } deriving (Show)
makeLenses ''Character
