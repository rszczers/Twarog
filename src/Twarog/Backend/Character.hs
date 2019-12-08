{-# LANGUAGE TemplateHaskell #-}

module Twarog.Backend.Character 
  ( Character (..)
  )
  where

import Data.Set
import Control.Lens

import Twarog.Backend.Types
import Twarog.Backend.Skills
import Twarog.Backend.Flaws
import Twarog.Backend.Talents
import Twarog.Backend.Item

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
  in Modifiers (f cha) (f con) (f dex) (f int) (f str) (f wil)      

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
