module Twarog.Backend.CharacterSheet
  (
  -- * Character sheet
    CharacterSheet
  -- ** Character lenses
  , playerName
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
  , Equipment (..)
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

  ) where

import Control.Lens
import qualified Data.Set as S

import Twarog.Backend.Archetypes
import Twarog.Backend.Gods
import Twarog.Backend.Item
import Twarog.Backend.Races
import Twarog.Backend.Types
import Twarog.Backend.Skills
import Twarog.Backend.SkillMods
import Twarog.Backend.Talents
import Twarog.Backend.Flaws
import Twarog.Backend.Character

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

{- | Character sheet, non-specific to combat mode, i.e. we 
don't consider e.g. Morale.
-}
data CharacterSheet = Player
  { _playerName  :: Owner
  , _charName    :: CharacterName 
  , _level       :: Lvl
  , _role        :: CharacterRole
  , _age         :: Age
  , _maxAge      :: Age
  , _race        :: Race
  , _height      :: Height
  , _size        :: Size
  , _lifeStance  :: LifeStance
  , _favGod      :: Maybe God
  , _alignment   :: Archetype
  , _stamina     :: SP
  , _health      :: HP
  , _sex         :: Sex
  , _combatStats :: CombatStats
  , _toughness   :: Toughness
  , _experience  :: XP
  , _resistance  :: Resistance
  , _flaws       :: [Flaw]
  , _talents     :: [Talent]
  , _skills      :: S.Set (CharacterSkill, SkillMod)
  , _equipment   :: Equipment
  , _attributes  :: Attributes
  , _other       :: [Note]
  } deriving (Show)
makeLenses ''CharacterSheet

-- | Default character sheet
emptySheet :: CharacterSheet
emptySheet = 
  let _playerName = ""
      _charName   = ""
      _level      = 0 
      _role       = Civilian 
      _age        = raceAdultAge HighMan
      _maxAge     = raceAdultAge HighMan
      _race       = HighMan
      _height     = raceHeight HighMan Male
      _size       = raceSizeMod HighMan Male
      _lifeStance = Traditional
      _favGod     = Nothing
      _alignment  = Kronic 
      _stamina    = 0
      _health     = 0
      _sex        = Male
      _combatStats = let _ovMe        = 0 
                         _ovMi        = 0 
                         _dvMe        = 0 
                         _dvMi        = 0 
                         _dodging     = 0 
                         _totalAv     = 0 
                         _msPenality  = 0 
                         _shieldDvMe  = 0 
                         _shieldBlock = \_ -> 0
                      in CombatStats {..}    
      _toughness = (Toughness 0 0 0 0)
      _experience = 0
      _resistance = Resistance 0 0
      _flaws = []
      _talents = []
      _skills = S.empty
      _equipment = let _belt          = Nothing 
                       _pouch         = Nothing 
                       _quiver        = Nothing 
                       _leftShoulder  = Nothing 
                       _rightShoulder = Nothing 
                       _sack          = Nothing 
                       _backpack      = Nothing 
                       _armour        = Nothing 
                       _helmet        = Nothing 
                       _shield        = Nothing 
                       _meleeWeapon   = Nothing 
                       _missileWeapon = Nothing 
                       _clothes       = Nothing 
                    in Equipment{..}
      _attributes = Attributes 0 0 0 0 0 0
      _other = []
    in Player {..}


