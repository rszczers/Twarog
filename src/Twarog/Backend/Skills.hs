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

class Applicable t
instance Applicable Morale
instance Applicable Encumbrance
instance Applicable Condition
instance Applicable CharacterSkill
instance Applicable Race

-- -- | Apply arbitrary modifier to given character 
-- applyMod :: Applicable t => t -> Character -> Character
-- applyMod Acrobatic
-- applyMod Aggresive
-- applyMod AnimalFriend
-- applyMod Arachnean
-- applyMod Argonautic
-- applyMod Ascetic
-- applyMod Athletic
-- applyMod Bloodhound
-- applyMod Calliopean
-- applyMod Careful
-- applyMod Caliopean
-- applyMod Courageous
-- applyMod Craftsman
-- applyMod Curious
-- applyMod DartThrower
-- applyMod DeepBreather
-- applyMod Dodger
-- applyMod Durable
-- applyMod Empathic
-- applyMod Enduring
-- applyMod Eratorean
-- applyMod Euterpean
-- applyMod Fast
-- applyMod FastSleeper
-- applyMod Favourite
-- applyMod Fearless
-- applyMod Fighter
-- applyMod FistFighter
-- applyMod Focused
-- applyMod GoodReflexes
-- applyMod HawkEyed
-- applyMod Hephaestusean
-- applyMod Heraklean
-- applyMod Herbalist
-- applyMod Humble
-- applyMod Inquisitive
-- applyMod Lancer
-- applyMod LightFooted
-- applyMod LynxEyes
-- applyMod Mariner
-- applyMod Marked
-- applyMod Mechanic
-- applyMod Malpogomeanean
-- applyMod Mermaid
-- applyMod Mule
-- applyMod Nimble
-- applyMod Perseusean
-- applyMod Pietistic
-- applyMod Polyhymnian
-- applyMod Rider
-- applyMod Sensitive
-- applyMod Sharpshooter
-- applyMod Shooter
-- applyMod Sirenean
-- applyMod Slinger
-- applyMod SlowAgeing
-- applyMod SpearThrower
-- applyMod Springy
-- applyMod StrongBack
-- applyMod StrongGrip
-- applyMod Survivor
-- applyMod Swimmer
-- applyMod SwordDancer
-- applyMod Terpsichorean
-- applyMod Thalian
-- applyMod Thrower
-- applyMod Tough
-- applyMod Tracker
-- applyMod TricksterTalent
-- applyMod Uranian
-- applyMod WarmHands
-- applyMod Zevsean
-- applyMod Aegirean

