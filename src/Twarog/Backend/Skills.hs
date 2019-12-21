module Twarog.Backend.Skills
  ( CharacterSkill (..)
  , characterSkill
  , skillMod
  , proficiency
  , Skill (..)
  , SkillType (..)
  , Proficiency (..)
  , skillType
  )
  where

import Control.Lens
import Twarog.Backend.Types

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

-- | Skills are grouped in following categories:
-- * CS -- Combat Skills
-- * SS -- Special Skills
-- * MS -- Movement Skills
data SkillType = MovementSkill
               | SpecialSkill
               | CombatSkill
               deriving (Eq, Ord, Show)

data CharacterSkill = CharacterSkill
  { _characterSkill :: Skill
  , _skillMod       :: Int
  , _proficiency    :: Proficiency
  } deriving (Eq, Ord, Show)
makeLenses ''CharacterSkill

-- | Assigns aprioprate category to given skill.
skillType :: Skill -> SkillType
skillType = \case
  Acrobatics         -> MovementSkill
  Acting             -> SpecialSkill
  Alchemy            -> SpecialSkill
  Climbing           -> MovementSkill
  Crafts             -> SpecialSkill
  Dancing            -> MovementSkill
  Dodging            -> MovementSkill
  FlutePlaying       -> SpecialSkill
  Foraging           -> SpecialSkill
  Fortitude          -> SpecialSkill
  Healing            -> SpecialSkill
  LyrePlaying        -> SpecialSkill
  Mechanics          -> SpecialSkill
  Melee              -> CombatSkill
  Missile            -> CombatSkill
  Navigation         -> SpecialSkill
  Perception         -> SpecialSkill
  Poetry             -> SpecialSkill
  ReligiousTradition -> SpecialSkill
  Riding             -> MovementSkill
  RuneLore           -> SpecialSkill
  Seamanship         -> SpecialSkill
  Singing            -> SpecialSkill
  SocialSkills       -> SpecialSkill
  Stamina            -> SpecialSkill
  Stealth            -> MovementSkill
  Swimming           -> MovementSkill
  Tempo              -> MovementSkill
  Tracking           -> SpecialSkill
  Trickery           -> SpecialSkill
  WorldLore          -> SpecialSkill


