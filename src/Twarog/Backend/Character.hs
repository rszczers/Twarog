module Twarog.Backend.Character 
  ( -- * Character
    CharacterRole (..)
  , NewCharacter (..)
  , emptyNewCharacter
  -- ** Character Role Utils
  , characterRoles 
  , availableRoles 
  -- ** Character lenses
  , characterOwner
  , characterName
  , characterAttr
  , characterRace
  , characterBirth
  , characterAlignment
  , characterLifeStance
  , characterSex  
  , characterHamingja
  , characterFlaws     
  , characterRole
  , characterSkills  
  , characterTalent
  , characterOther
  -- * Combat Statistics
  , CombatStats (..)
  , emptyCombatStats
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
  -- * Utilities
  , hp
  , expToLvl
  , lvlToExp
  , maximumAge
  , crHPBonus
  , defaultCrSkillChoices 
  , additionalTrainedSkills
  , crSkills 
  )
  where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens

import Twarog.Backend.Archetypes
import Twarog.Backend.Gods
import Twarog.Backend.Calendar
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
                   deriving (Eq, Enum)

instance Show CharacterRole where
  show Civilian      = "Civilian"
  show Warrior       = "Warrior"
  show Stalker       = "Stalker"
  show Trickster     = "Trickster"
  show Ranger        = "Ranger"
  show Bard          = "Bard"
  show Sorcerer      = "Sorcerer"
  show DwarfRole     = "Dwarf"
  show ElfRole       = "Elf"
  show GnomeRole     = "Gnome"
  show GoblinRole    = "Goblin"
  show HalflingRole  = "Halfling"
  show OrcRole       = "Orc"
  show HalfOrcRole   = "Half-orc"
  show OgreRole      = "Ogre"
  show HobgoblinRole = "Hobgoblin"

characterRoles :: [CharacterRole]
characterRoles = enumFrom (toEnum 0)

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

emptyCombatStats :: CombatStats
emptyCombatStats =
  let _ovMe = 0
      _ovMi = 0
      _dvMe = 0
      _dvMi = 0
      _dodging = 1
      _totalAv = 0
      _msPenality = 0
      _shieldDvMe = 0
      _shieldBlock = 0
   in CombatStats {..}

-- | Minimal character model; fields are written
-- in order of how forms should be completed
data NewCharacter = NewCharacter
  { _characterOwner      :: Maybe Owner
  , _characterName       :: Maybe Name
  , _characterAttr       :: Maybe Attributes
  , _characterRace       :: Maybe Race
  , _characterBirth      :: Maybe Birthday
  , _characterAlignment  :: Maybe Archetype
  , _characterLifeStance :: Maybe LifeStance
  , _characterSex        :: Maybe Sex
  , _characterHamingja   :: Maybe Hamingja
  , _characterFlaws      :: S.Set Flaw
  , _characterRole       :: Maybe CharacterRole
  , _characterSkills     :: M.Map Skill CharacterSkill
  , _characterTalent     :: S.Set Talent
  , _characterOther      :: [Note]
  } deriving (Show, Eq)
makeLenses ''NewCharacter
 
emptyNewCharacter = 
  let _characterOwner      = Nothing
      _characterName       = Nothing
      _characterAttr       = Nothing
      _characterRace       = Nothing
      _characterBirth      = Nothing
      _characterAlignment  = Nothing
      _characterLifeStance = Nothing
      _characterGod        = Nothing
      _characterSex        = Nothing
      _characterHamingja   = Just 3
      _characterFlaws      = S.empty
      _characterRole       = Nothing
      _characterSkills     = M.empty
      _characterTalent     = S.empty
      _characterOther      = []
   in NewCharacter{..}

-- | Maximal age that PC can live up to
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

-- | Check if for given parameters given Character Role is
-- allowed.
isProperRole :: Attributes -> [Talent]
             -> LifeStance -> Race -> Sex
             -> Archetype 
             -> CharacterRole 
             -> Bool
isProperRole (Attributes cha con dex int str wil) ts ls race sex arch cr =
  case cr of
    Civilian  -> race `elem` [CommonMan, LesserMan, HighMan]
    Warrior   -> con >= 9 
              && str >= 13 
              && wil >= 9 
              && sex == Male
              && race `elem` [CommonMan, LesserMan, HighMan]
    Stalker   -> con >= 9 
              && dex >= 9 
              && int >= 9
              && race `elem` [CommonMan, LesserMan, HighMan]
    Trickster -> con >= 9 
              && dex >= 9 
              && int >= 9
              && race `elem` [CommonMan, LesserMan, HighMan]
    Ranger    -> race == HighMan 
              && ls == (Religious Skadi)
              && arch == Artemisian
              && cha >= 13
              && con >= 9
              && dex >= 9
              && int >= 9
              && wil >= 9
              && race `elem` [CommonMan, LesserMan, HighMan]
    Bard      -> race == HighMan
              && ls /= Traditional
              && cha >= 16
              && Marked `elem` ts
              && race `elem` [CommonMan, LesserMan, HighMan]
    Sorcerer  -> race == HighMan
              && ls == Traditional
              && int >= 13
              && wil >= 13
              && Marked `elem` ts
              && race `elem` [CommonMan, LesserMan, HighMan]
    DwarfRole -> race == Dwarf
    ElfRole   -> race == Elf
    GnomeRole -> race == Gnome
    HalflingRole -> race == Halfling
    _ -> False

availableRoles :: Attributes -> [Talent]
               -> LifeStance -> Race -> Sex
               -> Archetype 
               -> [CharacterRole]
availableRoles attr ts ls rc sx arch =
  filter (isProperRole attr ts ls rc sx arch) characterRoles 

-- | Helper function for @expToLvl@
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

-- | Experience points to PC level conversion
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

-- | Gives back least XP boundary for given level
lvlToExp :: Race -> CharacterRole -> Sex
         -> Lvl -> XP
lvlToExp r cr s lvl =
  let nLvl = maxNormalLvl r cr s
   in if lvl <= nLvl
      then 250 * lvl * lvl
      else 500 * lvl * lvl 

-- | Default number of Character Skills from @crSkills@
-- that can be choosen.
defaultCrSkillChoices :: CharacterRole -> Int
defaultCrSkillChoices = \case
  Civilian -> 4
  _        -> 6

-- | Additional Trained Skills for male and female PC,
-- respectively
additionalTrainedSkills :: Sex -> [Skill]
additionalTrainedSkills = \case
  Male   -> [ Crafts
            , FlutePlaying
            , Foraging
            , LyrePlaying
            , Melee
            , Missile
            , Poetry
            , SocialSkills
            , Stamina
            , Swimming
            , WorldLore
            ]
  Female -> [ Acting
            , Crafts
            , Healing
            , Foraging
            , ReligiousTradition
            , Singing
            , SocialSkills
            , Swimming
            , WorldLore
            ]
  Non     -> []

-- | Player can choose his Character Skills
-- with regard to his PC role.
crSkills :: CharacterRole -> [Skill]
crSkills = \case
  Civilian      -> [ Acrobatics
                   , Acting
                   , Alchemy
                   , Climbing
                   , Crafts
                   , Dancing
                   , Dodging
                   , FlutePlaying
                   , Foraging
                   , Fortitude
                   , Healing
                   , LyrePlaying
                   , Mechanics
                   , Melee
                   , Missile
                   , Navigation
                   , Perception
                   , Poetry
                   , ReligiousTradition
                   , Riding
                   , RuneLore
                   , Seamanship
                   , Singing
                   , SocialSkills
                   , Stamina
                   , Stealth
                   , Swimming
                   , Tempo
                   , Tracking
                   , Trickery
                   , WorldLore
                   ]
  Warrior       -> [ Acrobatics
                   , Melee
                   , Missile
                   , Riding
                   , Seamanship
                   , Stamina
                   ]
  Stalker       -> [ Acrobatics
                   , Climbing
                   , Navigation
                   , Riding
                   , Stamina
                   , Stealth
                   , Swimming
                   , Tracking
                   ]
  Trickster     -> [ Acrobatics
                   , Acting
                   , Climbing
                   , Mechanics
                   , SocialSkills
                   , Stealth
                   , Trickery
                   ]
  Ranger        -> [ Acrobatics
                   , Foraging
                   , Healing
                   , Missile
                   , Navigation
                   , Poetry
                   , ReligiousTradition
                   , Stamina
                   , Stealth
                   , Swimming
                   , Tracking
                   ]
  Bard          -> [ Acting
                   , Alchemy
                   , Dancing
                   , FlutePlaying
                   , Healing
                   , LyrePlaying
                   , Poetry
                   , ReligiousTradition
                   , Singing
                   , SocialSkills
                   , WorldLore
                   ]
  Sorcerer      -> [ Alchemy
                   , Fortitude
                   , Healing
                   , Poetry
                   , RuneLore
                   , Singing
                   , Stamina
                   , WorldLore
                   ]
  DwarfRole     -> [ Climbing
                   , Crafts
                   , Foraging
                   , Fortitude
                   , Mechanics
                   , Melee
                   , Missile
                   , RuneLore
                   , Stamina
                   , WorldLore
                   ]
  ElfRole       -> [ Acrobatics
                   , Climbing
                   , Crafts
                   , Dancing
                   , FlutePlaying
                   , Foraging
                   , Fortitude
                   , Healing
                   , Melee
                   , Missile
                   , Navigation
                   , Poetry
                   , RuneLore
                   , Seamanship
                   , Singing
                   , Stealth
                   , Swimming
                   , Tracking
                   , WorldLore
                   ]
  GnomeRole     -> [ Alchemy
                   , Climbing
                   , Foraging
                   , Fortitude
                   , Poetry
                   , RuneLore
                   , Singing
                   , Stealth
                   , Trickery
                   , WorldLore
                   ]
  GoblinRole    -> [ Acrobatics
                   , Climbing
                   , Foraging
                   , Melee
                   , Missile
                   , Navigation
                   , Riding
                   , Stamina
                   , Stealth
                   , Tracking
                   ]
  HalflingRole  -> [ Climbing
                   , Dancing
                   , Foraging
                   , Fortitude
                   , Missile
                   , Navigation
                   , Poetry
                   , Singing
                   , SocialSkills
                   , Stealth
                   , Trickery
                   ]
  OrcRole       -> [ Acrobatics
                   , Climbing
                   , Foraging
                   , Melee
                   , Missile
                   , Stamina
                   , Tracking
                   ]
  HalfOrcRole   -> [ Acrobatics
                   , Climbing
                   , Foraging
                   , Mechanics
                   , Melee
                   , Missile
                   , Navigation
                   , Riding
                   ]
  OgreRole      -> [ Acrobatics
                   , Climbing
                   , Foraging
                   , Melee
                   , Stamina
                   , Tracking
                   ]
  HobgoblinRole -> [ Acrobatics
                   , Climbing
                   , Foraging
                   , Melee
                   , Stamina
                   , Tracking
                   ]

-- | Get health bonus points for gaining a level
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

-- | Compute maximal health points
hp :: CON -> Str -> Size -> CharacterRole -> Lvl -> HP
hp c str size cr lvl =
  let bonus = lvl * crHPBonus cr 0
   in str $ c + bonus + size
