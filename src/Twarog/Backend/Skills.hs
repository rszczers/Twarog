module Twarog.Backend.Skills
  ( CharacterSkill (..)
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
  , skillType      :: SkillType
  , proficiency    :: Proficiency
  } deriving (Eq, Ord, Show)

