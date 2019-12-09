{-# LANGUAGE TemplateHaskell #-}

module Twarog.Backend.Character 
  ( -- * Character
    Character
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
  -- ## CombatStats lenses
  , ovMe       
  , ovMi       
  , dvMe       
  , dvMi       
  , dodging    
  , totalAv    
  , msPenality 
  , shieldDvMe 
  , shieldBlock
  )
  where

import qualified Data.Set as S
import Control.Lens

import Twarog.Backend.Archetypes
import Twarog.Backend.Types
import Twarog.Backend.Skills
import Twarog.Backend.Flaws
import Twarog.Backend.Races
import Twarog.Backend.Talents
import Twarog.Backend.Item

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
  , _skills      :: S.Set CharacterSkill
  , _equipment   :: Equipment
  , _attributes  :: Attributes
  , _other       :: [String]
  } deriving (Show)
makeLenses ''Character

maximumAge :: Character -> Age
maximumAge c = case c ^. race of
  Dwarf     -> Mortal $ 30 * (c ^. attributes . con)
  Elf       -> Immortal
  Hobgoblin -> Mortal $ 30 * (c ^. attributes . con)
  HalfOrc   -> Mortal $ 30 * (c ^. attributes . con)
  Goblin    -> Mortal $ 30 * (c ^. attributes . con)
  Ogre      -> Mortal $ 30 * (c ^. attributes . con)
  CommonOrc -> Mortal $ 30 * (c ^. attributes . con)
  Gnome     -> Mortal $ 30 * (c ^. attributes . con)
  Halfling  -> Mortal $  6 * (c ^. attributes . con)
  CommonMan -> Mortal $  5 * (c ^. attributes . con)
  LesserMan -> Mortal $  4 * (c ^. attributes . con)
  HighMan   -> Mortal $  6 * (c ^. attributes . con)

acrobatics :: Character -> Int -> Int
acrobatics c =
  let baseMod = c ^. attributes . con . to toModifier
   in baseMod
