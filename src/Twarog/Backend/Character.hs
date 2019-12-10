{-# LANGUAGE TemplateHaskell #-}

module Twarog.Backend.Character 
  ( -- * Character
    CharacterSheet (..)
  , CharacterRole (..)
  -- ** Character lenses
  , playersName
  , charName   
  , level      
  , role       
  , age        
  , race       
  , height     
  , size       
  , lifeStance 
  , alignment  
  , stamina    
  , health     
  , toughness  
  , experience 
  , resistance 
  , flaws      
  , talents    
  , skills     
  , equipment  
  , attributes 
  , other      
  -- * Equipment
  , Equipment
  -- ** Equipment lenses
  , belt         
  , pouch        
  , quiver       
  , leftShoulder 
  , rightShoulder
  , sack         
  , backpack     
  , armour       
  , helmet       
  , shield       
  , meleeWeapon  
  , missileWeapon
  , clothes      
  -- * Combat Statistics
  , CombatStats
  -- ** CombatStats lenses
  , ovMe       
  , ovMi       
  , dvMe       
  , dvMi       
  , dodging    
  , totalAv    
  , msPenality 
  , shieldDvMe 
  , shieldBlock
  -- Utilities
  , expToLvl
  , maxNormalLvl
  , maximumAge
  , crHPBonus
  )
  where

import qualified Data.Set as S
import Control.Lens

import Twarog.Backend.Archetypes
import Twarog.Backend.Gods
import Twarog.Backend.Types
import Twarog.Backend.Skills
import Twarog.Backend.Flaws
import Twarog.Backend.Races
import Twarog.Backend.Talents
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
                   | GoblinRole
                   | HalflingRole
                   | OrcRole
                   | HalfOrcRole
                   | OgreRole
                   | HobgoblinRole
                   deriving (Show)

data CombatStats = CombatStats
  { _ovMe        :: OvMe
  , _ovMi        :: OvMi
  , _dvMe        :: DvMe
  , _dvMi        :: DvMi
  , _dodging     :: Dodging
  , _totalAv     :: TotalAv
  , _msPenality  :: MsPenality
  , _shieldDvMe  :: ShieldDvMe
  , _shieldBlock :: ShieldBlock
  } deriving (Show)
makeLenses ''CombatStats

data Equipment = Equipment
  { _belt          :: Maybe Bag
  , _pouch         :: Maybe Bag
  , _quiver        :: Maybe Bag
  , _leftShoulder  :: Maybe Bag
  , _rightShoulder :: Maybe Bag
  , _sack          :: Maybe Bag
  , _backpack      :: Maybe Bag
  , _armour        :: Maybe (Item (Armour BodyArmour))
  , _helmet        :: Maybe (Item (Armour Helmet))
  , _shield        :: Maybe (Item Shield)
  , _meleeWeapon   :: Maybe (Item Weapon)
  , _missileWeapon :: Maybe (Item Weapon)
  , _clothes       :: Maybe Bag
  } deriving (Show)
makeLenses ''Equipment

instance Show Modifiers where
  show (Modifiers a b c d e f) =
    let xs = [a, b, c, d, e, f] <*> [0]
     in show xs

{- | Character sheet, non-specific to combat mode, i.e. we 
don't consider e.g. Morale.
-}
data CharacterSheet = Player
  { _playersName :: String
  , _charName    :: String
  , _level       :: Lvl
  , _role        :: CharacterRole
  , _age         :: Age
  , _race        :: Race
  , _height      :: Height
  , _size        :: Size
  , _lifeStance  :: LifeStance
  , _favGod      :: Maybe God
  , _alignment   :: Archetype
  , _stamina     :: SP
  , _health      :: HP
  , _toughness   :: Toughness
  , _experience  :: XP
  , _resistance  :: Resistance
  , _flaws       :: [Flaw]
  , _talents     :: [Talent]
  , _skills      :: S.Set CharacterSkill
  , _equipment   :: Equipment
  , _attributes  :: Attributes
  , _other       :: [String]
  } deriving (Show)
makeLenses ''CharacterSheet

maximumAge :: Race -> CON -> Age
maximumAge race con = case race of
  Dwarf     -> Mortal $ 30 *  con
  Elf       -> Immortal
  Hobgoblin -> Mortal $ 30 * con
  HalfOrc   -> Mortal $ 30 * con
  Goblin    -> Mortal $ 30 * con
  Ogre      -> Mortal $ 30 * con
  CommonOrc -> Mortal $ 30 * con
  Gnome     -> Mortal $ 30 * con
  Halfling  -> Mortal $  6 * con
  CommonMan -> Mortal $  5 * con
  LesserMan -> Mortal $  4 * con
  HighMan   -> Mortal $  6 * con

isProperRole :: CharacterRole 
             -> Attributes -> [Talent]
             -> LifeStance -> Race -> Sex -> God
             -> Archetype 
             -> Bool
isProperRole cr (Attributes cha con dex int str wil) ts ls race sex god arch =
  case cr of
    Civilian -> True
    Warrior   -> con >= 9 
              && str >= 13 
              && wil >= 9 
              && sex == Male
    Stalker   -> con >= 9 
              && dex >= 9 
              && int >= 9
    Trickster -> con >= 9 
              && dex >= 9 
              && int >= 9
    Ranger    -> race == HighMan 
              && ls == Religious 
              && arch == Artemisian
              && god == Skadi
              && cha >= 13
              && con >= 9
              && dex >= 9
              && int >= 9
              && wil >= 9
    Bard      -> race == HighMan
              && ls == Religious
              && cha >= 16
              && Marked `elem` ts
    Sorcerer  -> race == HighMan
              && ls == Traditional
              && int >= 13
              && wil >= 13
              && Marked `elem` ts
    DwarfRole -> race == Dwarf
    ElfRole   -> race == Elf
    GnomeRole -> race == Gnome
    HalflingRole -> race == Halfling
    _ -> False

maxNormalLvl :: Race -> CharacterRole -> Sex -> Lvl
maxNormalLvl race cr sex = case race of
  Dwarf       -> 9
  Elf         -> 9
  Hobgoblin   -> 6
  HalfOrc     -> 9
  Goblin      -> 6
  Ogre        -> 5
  CommonOrc   -> 6
  Gnome       -> 7
  Halfling    -> 6
  HighMan     -> case cr of
    Bard      -> case sex of
      Male    -> 12
      Female  -> 14
    Civilian  -> 12
    Ranger    -> 12
    Sorcerer  -> 12
    Stalker   -> 12
    Trickster -> case sex of
      Male    -> 12
      Female  -> 14
    Warrior   -> 12
  _           -> case cr of
    Civilian  -> 12
    Stalker   -> 12
    Trickster -> case sex of
      Male    -> 12
      Female  -> 14
    Warrior   -> 12
    _         -> error "Only High Man can be a Bard, Ranger or Sorcerer"

expToLvl :: Race -> CharacterRole -> Sex
         -> XP -> Lvl
expToLvl r cr s xp =
  let squareRoot = floor . sqrt . (fromIntegral :: Int -> Double) -- ffs
      lvl = squareRoot $ xp `div` 250
      lvl' = squareRoot $ xp `div` 500
      m = fromIntegral $ maxNormalLvl r cr s
      p = lvl <= m
      dlvl = lvl - m
   in fromInteger $ if p 
      then lvl
      else lvl'

crHPBonus :: CharacterRole -> Int -> Int
crHPBonus = \case
  Civilian      -> (+ 1)
  Warrior       -> (+ 2)   
  Stalker       -> (+ 1)       
  Trickster     -> (+ 1) 
  Ranger        -> (+ 1)    
  Bard          -> (+ 1)      
  Sorcerer      -> (+ 1)  
  DwarfRole     -> (+ 3) 
  ElfRole       -> (+ 1)   
  GnomeRole     -> (+ 1) 
  HalflingRole  -> (+ 1)
  OrcRole       -> (+ 2)
  GoblinRole    -> (+ 1)
  HobgoblinRole -> (+ 3)
  HalfOrcRole   -> (+ 2)
  OgreRole      -> (+ 3)

sp :: CharacterRole -> Con -> Lvl 
   -> Int
sp cr c lvl = 
  let bonus = (* lvl) . crHPBonus cr 
   in bonus . c $ 8

hp :: CON -> Str -> Size -> Lvl
   -> Int
hp c str s lvl = str . s $ c
