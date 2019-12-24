module Twarog.Backend.Skills
  ( CharacterSkill (..)
  , skillMod
  , proficiency
  , Skill (..)
  , skills
  , SkillType (..)
  , Proficiency (..)
  , skillType
  , typeSkill
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
           deriving (Eq, Ord, Enum, Show)

skills :: [Skill]
skills = enumFrom $ toEnum 0

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
  { _skillMod       :: Int
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

-- | List all skills of given type
typeSkill :: SkillType -> [Skill]
typeSkill t = filter (\x -> skillType x == t) skills 
