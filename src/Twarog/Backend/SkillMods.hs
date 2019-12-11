module Twarog.Backend.SkillMods
  ( 
    SkillMod (..)
  , untrainedPenality
  )
  where

import Control.Lens
import Data.Set

import Twarog.Backend.Types (Lvl)
import Twarog.Backend.Character
import Twarog.Backend.Calendar
import Twarog.Backend.Skills
import Twarog.Backend.Talents
import Twarog.Backend.Races
import Twarog.Backend.Flaws

untrainedPenality :: Skill -> Int -> Int
untrainedPenality skill =
  let y = case skill of
            Acting -> 2
            Alchemy -> 5 
            Crafts -> 5
            Dancing -> 2
            FlutePlaying -> 5
            Foraging -> 2
            Healing -> 5
            LyrePlaying -> 5
            Mechanics -> 5
            Melee -> 2
            Missile -> 2
            Navigation -> 2
            Poetry -> 5
            ReligiousTradition -> 5
            Riding -> 5
            RuneLore -> 5
            Seamanship -> 5
            Singing -> 5
            SocialSkills -> 2
            Swimming -> 5
            Tracking -> 2
            Trickery -> 2
            WorldLore -> 5
            _ -> 0
   in \x -> x - y  

crSkillMod :: Lvl -> Int -> Int
crSkillMod level =
  let m = level `div` 2
   in \x -> x + (min m 5)

trainedSkillMod :: Lvl -> Int -> Int
trainedSkillMod level =
  let m = level `div` 4
   in \x -> x + (min m 4)

data SkillMod a = AcrobaticsMod a
                | ActingMod a
                | AlchemyMod a
                | ClimbingMod a
                | CraftsMod a
                | DancingMod a
                | DodgingMod a
                | FlutePlayingMod a
                | ForagingMod a
                | FortitudeMod a
                | HealingMod a
                | LyrePlayingMod a
                | MechanicsMod a
                | MeleeMod a
                | MissileMod a
                | NavigationMod a
                | PerceptionMod a
                | PoetryMod a
                | ReligiousTraditionMod a
                | RidingMod a
                | RuneLoreMod a
                | SeamanshipMod a
                | SingingMod
                | SocialSkillsMod a
                | StaminaMod a
                | StealthMod a
                | SwimmingMod a
                | TempoMod a
                | TrackingMod a
                | TrickeryMod a
                | WorldLoreMod a
                deriving (Show, Eq)

-- skillMod :: Character -> Skill -> Int -> Int
-- skillMod c = \case
--   Acrobatics ->
--   Acting ->
--   Alchemy ->
--   Climbing ->
--   Crafts ->
--   Dancing ->
--   Dodging ->
--   FlutePlaying ->
--   Foraging ->
--   Fortitude ->
--   Healing ->
--   LyrePlaying ->
--   Mechanics ->
--   Melee ->
--   Missile ->
--   Navigation ->
--   Perception ->
--   Poetry ->
--   ReligiousTradition ->
--   Riding ->
--   RuneLore ->
--   Seamanship ->
--   Singing ->
--   SocialSkills ->
--   Stamina ->
--   Stealth ->
--   Swimming ->
--   Tempo ->
--   Tracking ->
--   Trickery ->
--   WorldLore ->

