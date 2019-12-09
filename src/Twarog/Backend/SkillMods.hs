module Twarog.Backend.SkillMods
  ( 
    skillMod
  , untrainedPenality
  )
  where

import Control.Lens
import Data.Set

import Twarog.Backend.Character
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

crSkillMod :: Character -> Int -> Int
crSkillMod c =
  let m = c ^. level . to (`div` 2)
   in \x -> x + (min m 5)

trainedSkillMod :: Character -> Int -> Int
trainedSkillMod c =
  let m = c ^. level . to (`div` 4)
   in \x -> x + (min m 4)

skillMod = undefined

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

