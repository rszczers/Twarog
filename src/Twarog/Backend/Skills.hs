module Twarog.Backend.Skills
  ( CharacterSkill (..)
  , Skill (..)
  , SkillType (..)
  , SkillMod (..)
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

-- | Skills are grouped in following categories:
-- * CS -- Combat Skills
-- * SS -- Special Skills
-- * MS -- Movement Skills
data SkillType = MovementSkill
               | SpecialSkill
               | CombatSkill
               deriving (Eq, Ord, Show)

data CharacterSkill = CharacterSkill
  { characterSkill :: Skill
  , proficiency    :: Proficiency
  } deriving (Eq, Ord, Show)

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

data SkillMod = AcrobaticsMod Mod
              | ActingMod Mod
              | AlchemyMod Mod
              | ClimbingMod Mod
              | CraftsMod Mod
              | DancingMod Mod
              | DodgingMod Mod
              | FlutePlayingMod Mod
              | ForagingMod Mod
              | FortitudeMod Mod
              | HealingMod Mod
              | LyrePlayingMod Mod
              | MechanicsMod Mod
              | MeleeMod Mod
              | MissileMod Mod
              | NavigationMod Mod
              | PerceptionMod Mod
              | PoetryMod Mod
              | ReligiousTraditionMod Mod
              | RidingMod Mod
              | RuneLoreMod Mod
              | SeamanshipMod Mod
              | SingingMod Mod
              | SocialSkillsMod Mod
              | StaminaMod Mod
              | StealthMod Mod
              | SwimmingMod Mod
              | TempoMod Mod
              | TrackingMod Mod
              | TrickeryMod Mod
              | WorldLoreMod Mod
              deriving (Show)

