module Twarog.Backend.SkillMods
  ( untrainedPenality
  , baseSkillMod
  , setBaseSkill
  , setPenalitySkill 
  ) where

import Control.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Twarog.Backend.Types
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

baseSkillMod :: Modifiers -> Skill -> Int
baseSkillMod m@Modifiers{..} = \case
  Acrobatics -> _dexMod 0
  Acting -> _chaMod 0
  Alchemy -> _intMod 0
  Climbing -> _dexMod 0
  Crafts -> _intMod 0
  Dancing -> _dexMod 0
  Dodging -> (_dexMod 0) `div` 2
  FlutePlaying -> _intMod 0
  Foraging -> _intMod 0
  Fortitude -> _wilMod 0
  Healing -> _intMod 0
  LyrePlaying -> _intMod 0
  Mechanics -> _dexMod 0
  Melee -> _strMod 0
  Missile -> _dexMod 0
  Navigation -> _intMod 0
  Perception -> _intMod 0
  Poetry -> _intMod 0
  ReligiousTradition -> _chaMod 0
  Riding -> _dexMod 0
  RuneLore -> _intMod 0
  Seamanship -> _intMod 0
  Singing -> _intMod 0
  SocialSkills -> _chaMod 0
  Stamina -> 8 + _conMod 0
  Stealth -> _dexMod 0
  Swimming -> _conMod 0
  Tempo -> 40 + 5 *  _strMod 0
  Tracking -> _intMod 0
  Trickery -> _dexMod 0 
  WorldLore -> _intMod 0

-- | Set base skill mod for all skills
setBaseSkill :: Modifiers
             -> M.Map Skill CharacterSkill
             -> M.Map Skill CharacterSkill
setBaseSkill m cs = foldr (\s cs -> 
  cs & at s . _Just . skillMod +~ baseSkillMod m s) cs skills

-- | Set penality according to skill proficiency
setPenalitySkill :: M.Map Skill CharacterSkill -> M.Map Skill CharacterSkill
setPenalitySkill cs =
  let pen p s = if p == Untrained
                then untrainedPenality s
                else id
   in foldr 
        (\s cs -> 
           if preview (_Just . proficiency) (cs ^. at s) == Just Untrained
           then cs & at s . _Just . skillMod %~ untrainedPenality s
           else cs
        ) cs skills
