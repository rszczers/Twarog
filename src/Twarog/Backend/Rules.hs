module Twarog.Backend.Rules
  -- ( applyRule
  -- , Applicable
  -- )
  where

import Control.Lens

import Twarog.Backend.Types
import Twarog.Backend.Character
import Twarog.Backend.Skills

class Applicable t
instance Applicable Morale
instance Applicable Encumbrance
instance Applicable Condition
instance Applicable CharacterSkill
instance Applicable Race

adultAge :: Race -> Age
adultAge = \case
  Dwarf -> Mortal 21
  Elf -> Mortal 28
  Hobgoblin -> Mortal 14
  HalfOrc -> Mortal 14
  Goblin -> Mortal 14
  Ogre -> Mortal 14 
  CommonOrc -> Mortal 14
  Gnome -> Mortal 21
  Halfling -> Mortal 21
  CommonMan -> Mortal 21
  LesserMan -> Mortal 14
  HighMan -> Mortal 21

maximumAge :: Character -> Age
maximumAge c = case c ^. race of
  Dwarf -> Mortal $ 30 * (c ^. attributes . con)
  Elf -> Immortal
  Hobgoblin -> Mortal $ 30 * (c ^. attributes . con)
  HalfOrc -> Mortal $ 30 * (c ^. attributes . con)
  Goblin -> Mortal $ 30 * (c ^. attributes . con)
  Ogre -> Mortal $ 30 * (c ^. attributes . con)
  CommonOrc -> Mortal $ 30 * (c ^. attributes . con)
  Gnome -> Mortal $ 30 * (c ^. attributes . con)
  Halfling -> Mortal $ 6 * (c ^. attributes . con)
  CommonMan -> Mortal $ 5 * (c ^. attributes . con)
  LesserMan -> Mortal $ 4 * (c ^. attributes . con)
  HighMan -> Mortal $ 6 * (c ^. attributes . con)


-- -- | Apply arbitrary modifier to given character 
-- applyRule  :: Applicable t => t -> Character -> Character
-- applyRule Acrobatic 
-- applyRule Aggresive
-- applyRule AnimalFriend
-- applyRule Arachnean
-- applyRule Argonautic
-- applyRule Ascetic
-- applyRule Athletic
-- applyRule Bloodhound
-- applyRule Calliopean
-- applyRule Careful
-- applyRule Caliopean
-- applyRule Courageous
-- applyRule Craftsman
-- applyRule Curious
-- applyRule DartThrower
-- applyRule DeepBreather
-- applyRule Dodger
-- applyRule Durable
-- applyRule Empathic
-- applyRule Enduring
-- applyRule Eratorean
-- applyRule Euterpean
-- applyRule Fast
-- applyRule FastSleeper
-- applyRule Favourite
-- applyRule Fearless
-- applyRule Fighter
-- applyRule FistFighter
-- applyRule Focused
-- applyRule GoodReflexes
-- applyRule HawkEyed
-- applyRule Hephaestusean
-- applyRule Heraklean
-- applyRule Herbalist
-- applyRule Humble
-- applyRule Inquisitive
-- applyRule Lancer
-- applyRule LightFooted
-- applyRule LynxEyes
-- applyRule Mariner
-- applyRule Marked
-- applyRule Mechanic
-- applyRule Malpogomeanean
-- applyRule Mermaid
-- applyRule Mule
-- applyRule Nimble
-- applyRule Perseusean
-- applyRule Pietistic
-- applyRule Polyhymnian
-- applyRule Rider
-- applyRule Sensitive
-- applyRule Sharpshooter
-- applyRule Shooter
-- applyRule Sirenean
-- applyRule Slinger
-- applyRule SlowAgeing
-- applyRule SpearThrower
-- applyRule Springy
-- applyRule StrongBack
-- applyRule StrongGrip
-- applyRule Survivor
-- applyRule Swimmer
-- applyRule SwordDancer
-- applyRule Terpsichorean
-- applyRule Thalian
-- applyRule Thrower
-- applyRule Tough
-- applyRule Tracker
-- applyRule TricksterTalent
-- applyRule Uranian
-- applyRule WarmHands
-- applyRule Zevsean
-- applyRule Aegirean

