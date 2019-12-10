module Twarog.Backend.Races
  ( -- * Fictional races
    Race (..)
  -- ** Characteristics
  , raceHeight
  , raceAdultAge
  , raceSizeMod
  , raceAttrMod
  -- ** Toughness mods
  , raceColdMod
  , raceHeatMod
  -- ** Resistance mods
  , raceDiseaseMod
  , racePoisonMod
  -- ** Skill mods
  , raceSkillMod
  )
  where

import Control.Lens

import Twarog.Backend.Types
import Twarog.Backend.Units
import Twarog.Backend.Skills

data Race = Dwarf
          | Elf
          | Hobgoblin
          | HalfOrc
          | Goblin
          | Ogre
          | CommonOrc
          | Gnome
          | Halfling
          | CommonMan
          | LesserMan
          | HighMan
          deriving (Eq, Show)

raceSizeMod :: Race -> Sex -> Size
raceSizeMod race sex = case race of
  Dwarf     -> (\x -> x - 1)
  Elf       -> (\x -> x - 1)
  Hobgoblin -> (+ 1)
  Goblin    -> (\x -> x - 2)
  Ogre      -> (+ 3)
  Gnome     -> (\x -> x - 3)
  Halfling  -> (\x -> x - 3)
  CommonMan -> case sex of
    Male    -> id
    Female  -> (\x -> x - 1)
  LesserMan -> case sex of
    Male    -> id
    Female  -> (\x -> x - 1)
  HighMan   -> case sex of
    Male    -> (+ 1)
    Female  -> (\x -> x - 1)
  _         -> id

raceAdultAge :: Race -> Age
raceAdultAge = \case
  Dwarf     -> Mortal 21
  Elf       -> Mortal 28
  Hobgoblin -> Mortal 14
  HalfOrc   -> Mortal 14
  Goblin    -> Mortal 14
  Ogre      -> Mortal 14
  CommonOrc -> Mortal 14
  Gnome     -> Mortal 21
  Halfling  -> Mortal 21
  CommonMan -> Mortal 21
  LesserMan -> Mortal 14
  HighMan   -> Mortal 21

raceAttrMod :: Race -> Sex -> Attributes -> Attributes
raceAttrMod race sex attr = case race of
  Dwarf     -> attr & (cha -~ 2)
                    . (con +~ 3)
                    . (dex -~ 1)
                    . (wil +~ 2)
  Elf       -> attr & (cha +~ 3)
                    . (con -~ 1)
                    . (dex +~ 3)
                    . (int +~ 1)
                    . (str -~ 1)
                    . (wil +~ 1)
  Hobgoblin -> attr & (cha -~ 3)
                    . (con +~ 4)
                    . (int -~ 1)
                    . (str +~ 1)
                    . (wil -~ 1)
  HalfOrc   -> attr & (cha -~ 2)
                    . (con +~ 1)
                    . (dex -~ 1)
                    . (int -~ 1)
                    . (wil -~ 1)
  Goblin    -> attr & (cha -~ 3)
                    . (con +~ 2)
                    . (int -~ 2)
                    . (str -~ 1)
                    . (wil -~ 2)
  Ogre      -> attr & (cha +~ 3)
                    . (con +~ 3)
                    . (dex -~ 1)
                    . (int -~ 3)
                    . (str +~ 4)
                    . (wil -~ 2)
  CommonOrc -> attr & (cha -~ 3)
                    . (con +~ 3)
                    . (dex -~ 1)
                    . (int -~ 2)
                    . (wil -~ 2)
  Gnome     -> attr & (cha -~ 3)
                    . (con +~ 1)
                    . (dex +~ 1)
                    . (int +~ 2)
                    . (str -~ 4)
  Halfling  -> attr & (cha -~ 1)
                    . (con +~ 3)
                    . (dex +~ 3)
                    . (str -~ 4)
  CommonMan -> case sex of
    Male    -> attr -- Nothing changes
    Female  -> attr & (cha +~ 2)
                    . (str -~ 2)
  LesserMan -> case sex of
    Male    -> attr & (cha -~ 1)
                    . (int -~ 1)
                    . (str -~ 1)
                    . (wil -~ 1)
    Female  -> attr & (int -~ 1)
                    . (str -~ 2)
                    . (wil -~ 1)
  HighMan   -> case sex of
    Male    -> attr & (cha +~ 1)
                    . (con +~ 2)
                    . (wil +~ 1)
    Female  -> attr & (cha +~ 3)
                    . (con +~ 2)
                    . (str -~ 2)
                    . (wil +~ 1)

raceHeight :: Race -> Sex -> Height
raceHeight race sex = case race of
  Dwarf     -> (toDistance @Inch $ foot 4) <> inch 6
  Elf       -> (toDistance @Inch $ foot 5) <> inch 4
  Hobgoblin -> toDistance @Inch $ foot 6
  HalfOrc   -> (toDistance @Inch $ foot 5) <> inch 6
  Goblin    -> toDistance @Inch $ foot 4
  Ogre      -> toDistance @Inch $ foot 8
  CommonOrc -> toDistance @Inch $ foot 5
  Gnome     -> toDistance @Inch $ foot 3
  Halfling  -> toDistance @Inch $ foot 3
  CommonMan -> case sex of
    Male    -> (toDistance @Inch $ foot 5) <> inch 10
    Female  -> (toDistance @Inch $ foot 5) <> inch 6
  LesserMan -> case sex of
    Male    -> (toDistance @Inch $ foot 5) <> inch 8
    Female  -> (toDistance @Inch $ foot 5) <> inch 5
  HighMan   -> case sex of
    Male    -> toDistance @Inch $ foot 6
    Female  -> (toDistance @Inch $ foot 5) <> inch 7

raceColdMod :: Race -> Int -> Int
raceColdMod = \case
  Dwarf    -> (+ 3)
  Elf      -> (+ 2)
  Gnome    -> (+ 2)
  Halfling -> (+ 2)
  HighMan  -> (+ 1)
  _ -> id

raceHeatMod :: Race -> Int -> Int
raceHeatMod = \case
  Dwarf     -> (+ 3)
  Hobgoblin -> (+ 2)
  HalfOrc   -> (+ 1)
  Goblin    -> (+ 2)
  Ogre      -> (+ 2)
  CommonOrc -> (+ 2)
  Gnome     -> (+ 2)
  Halfling  -> (+ 2)
  LesserMan -> (+ 1)
  _ -> id

raceDiseaseMod :: Race -> Int -> Int
raceDiseaseMod = \case
  Dwarf     -> (+ 1)
  Elf       -> (+ 10)
  Hobgoblin -> (+ 1)
  Goblin    -> (+ 1)
  Ogre      -> (+ 1)
  CommonOrc -> (+ 1)
  Gnome     -> (+ 1)
  Halfling  -> (+ 3)
  LesserMan -> (\x -> x - 1)
  HighMan   -> (+ 1)
  _ -> id

racePoisonMod :: Race -> Int -> Int
racePoisonMod = \case 
  Dwarf     -> (+ 1)
  Elf       -> (+ 1)
  Hobgoblin -> (+ 2)
  HalfOrc   -> (+ 1)
  Goblin    -> (+ 2)
  Ogre      -> (+ 2)
  CommonOrc -> (+ 2)
  Gnome     -> (+ 1)
  Halfling  -> (+ 2)
  _ -> id

raceSkillMod :: Race -> Skill -> Int -> Int
raceSkillMod race skill = case race of
  Dwarf -> case skill of
    Crafts ->    (+ 2)
    Fortitude -> (+ 4)
    Mechanics -> (+ 1)
    RuneLore ->  (+ 1)
    Stamina ->   (+ 4)
    Swimming ->  (\x -> x - 5)
    _ -> id
  Elf -> case skill of
    Acrobatics -> (+ 1)
    Crafts ->     (+ 1)
    Fortitude ->  (+ 2)
    Missile ->    (+ 1)
    Perception -> (+ 2)
    RuneLore ->   (+ 2)
    Stealth ->    (+ 2)
    _ -> id
  Hobgoblin -> case skill of
    Perception -> (+ 1)
    Stamina ->    (+ 12)
    Tracking ->   (+ 2)
    _ -> id
  HalfOrc -> case skill of
    Stamina ->  (+ 6)
    Tracking -> (+ 1)
    _ -> id
  Goblin -> case skill of
    Perception -> (+ 1)
    Stamina ->    (+ 12)
    Stealth ->    (+ 1)
    Tracking ->   (+ 4)
    _ -> id
  Ogre -> case skill of
    Climbing -> (+ 1)
    Stamina ->  (+ 12) 
    Tracking -> (+ 2)
    _ -> id
  CommonOrc -> case skill of
    Perception -> (+ 1)
    Stamina ->    (+ 12)
    Tracking ->   (+ 2)
    _ -> id
  Gnome -> case skill of
    Fortitude ->  (+ 5)
    Perception -> (+ 1)
    Stealth ->    (+ 1)
    Swimming ->   (\x -> x - 5)
    _ -> id
  Halfling -> case skill of
    Fortitude ->  (+ 7)
    Missile ->    (+ 1)
    Perception -> (+ 1)
    Stealth ->    (+ 3)
    Swimming ->   (\x -> x - 2)
    _ -> id
  _ -> id

