module Twarog.Backend.CharacterSheet
  (
  -- * Character sheet
    CharacterSheet (..)
  -- ** Character lenses
  , sheetPlayerName
  , sheetCharName
  , sheetLevel
  , sheetRole
  , sheetMaxAge
  , sheetAge
  , sheetRace
  , sheetHeight
  , sheetSize
  , sheetLifeStance
  , sheetCombatStats
  , sheetAlignment
  , sheetSex
  , sheetHealth
  , sheetToughness
  , sheetExperience
  , sheetEncumbrance
  , sheetResistance
  , sheetFlaws
  , sheetFright
  , sheetTalents
  , sheetSkills
  , sheetEquipment
  , sheetAttributes
  , sheetOther
  -- * Equipment
  , Equipment (..)
  , emptyEquipment
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
import qualified Data.Map as M
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

emptyEquipment :: Equipment
emptyEquipment = 
  let _belt = Nothing
      _pouch = Nothing
      _quiver = Nothing
      _leftShoulder = Nothing
      _rightShoulder = Nothing
      _sack = Nothing
      _backpack = Nothing
      _armour = Nothing
      _helmet = Nothing
      _shield = Nothing
      _meleeWeapon = Nothing
      _missileWeapon = Nothing
      _clothes = Nothing
   in Equipment{..}
  
{- | Character sheet, non-specific to combat mode, i.e. we
don't consider e.g. Morale.
-}
data CharacterSheet = Player
  { _sheetPlayerName  :: Owner
  , _sheetCharName    :: CharacterName
  , _sheetLevel       :: Lvl
  , _sheetRole        :: CharacterRole
  , _sheetAge         :: Age
  , _sheetMaxAge      :: Age
  , _sheetRace        :: Race
  , _sheetHeight      :: Height
  , _sheetSize        :: Size
  , _sheetLifeStance  :: LifeStance
  , _sheetAlignment   :: Archetype
  , _sheetHealth      :: HP
  , _sheetSex         :: Sex
  , _sheetHamingja    :: Hamingja
  , _sheetCombatStats :: CombatStats
  , _sheetToughness   :: Toughness
  , _sheetExperience  :: XP
  , _sheetEncumbrance :: Float
  , _sheetResistance  :: Resistance
  , _sheetFlaws       :: S.Set Flaw
  , _sheetTalents     :: S.Set Talent
  , _sheetSkills      :: M.Map Skill CharacterSkill
  , _sheetEquipment   :: Equipment
  , _sheetAttributes  :: Attributes
  , _sheetFright      :: Fright
  , _sheetOther       :: [Note]
  } deriving (Show)
makeLenses ''CharacterSheet

-- | Default character sheet
emptySheet :: CharacterSheet
emptySheet =
  let _sheetPlayerName  = ""
      _sheetCharName    = ""
      _sheetLevel       = 0
      _sheetRole        = Civilian
      _sheetAge         = raceAdultAge HighMan
      _sheetMaxAge      = raceAdultAge HighMan
      _sheetRace        = HighMan
      _sheetHeight      = raceHeight HighMan Male
      _sheetSize        = raceSizeMod HighMan Male 0
      _sheetLifeStance  = Traditional
      _sheetFavGod      = Nothing
      _sheetAlignment   = Kronic
      _sheetStamina     = 0
      _sheetHealth      = 0
      _sheetSex         = Male
      _sheetHamingja    = 3
      _sheetEncumbrance = 0
      _sheetCombatStats = emptyCombatStats
      _sheetToughness   = Toughness 0 0 0 0
      _sheetExperience  = 0
      _sheetResistance  = Resistance 0 0
      _sheetFlaws       = S.empty
      _sheetTalents     = S.empty 
      _sheetSkills      = M.empty
      _sheetEquipment   = emptyEquipment
      _sheetAttributes  = Attributes 0 0 0 0 0 0
      _sheetFright      = 0
      _sheetOther       = []
    in Player {..}
