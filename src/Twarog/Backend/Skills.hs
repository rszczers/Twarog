module Twarog.Backend.Skills
  ( CharacterSkill (..)
  , Skill (..)
  , SkillType (..)
  , Proficiency (..)
  , skillType
  )
  where

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

data SkillType = MovementSkill
               | SpecialSkill
               | CombatSkill
               deriving (Eq, Ord, Show)

data CharacterSkill = CharacterSkill
  { characterSkill :: Skill
  , proficiency    :: Proficiency
  } deriving (Eq, Ord, Show)

skillType :: Skill -> SkillType
skillType = \case
  Acrobatics -> MovementSkill
  Acting -> SpecialSkill
  Alchemy -> SpecialSkill
  Climbing -> MovementSkill
  Crafts -> SpecialSkill
  Dancing -> MovementSkill
  Dodging -> MovementSkill
  FlutePlaying -> SpecialSkill
  Foraging -> SpecialSkill
  Fortitude -> SpecialSkill
  Healing -> SpecialSkill
  LyrePlaying -> SpecialSkill
  Mechanics -> SpecialSkill
  Melee -> CombatSkill
  Missile -> CombatSkill
  Navigation -> SpecialSkill
  Perception -> SpecialSkill
  Poetry -> SpecialSkill
  ReligiousTradition -> SpecialSkill
  Riding -> MovementSkill
  RuneLore -> SpecialSkill
  Seamanship -> SpecialSkill
  Singing -> SpecialSkill
  SocialSkills -> SpecialSkill
  Stamina -> SpecialSkill
  Stealth -> MovementSkill
  Swimming -> MovementSkill
  Tempo -> MovementSkill
  Tracking -> SpecialSkill
  Trickery -> SpecialSkill
  WorldLore -> SpecialSkill

