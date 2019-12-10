module Twarog.Backend.Calendar
  ( Month (..)
  , Season (..)
  , month
  , monthGod
  , monthSeason
  , seasonAttrMod
  ) where

import Control.Lens 

import Twarog.Backend.Gods
import Twarog.Backend.Types

data Month = Valaskjolf
           | Himinbjorg
           | Landvidi
           | Sokkvabekkr
           | Thrudheimr
           | Breidablik
           | Noatun
           | Glitnir
           | Folkvangr
           | Alfheimr
           | Gladsheimr
           | Thrymheimr
           | Ydalir
           deriving (Ord, Eq, Show)

data Season = Winter
            | Spring
            | Summer
            | Autumn
            | NewYear
            deriving (Eq, Show)

month :: Int -> Month
month = \case
  1  -> Valaskjolf
  2  -> Himinbjorg
  3  -> Landvidi
  4  -> Sokkvabekkr
  5  -> Thrudheimr
  6  -> Breidablik
  7  -> Noatun
  8  -> Glitnir
  9  -> Folkvangr
  10 -> Alfheimr
  11 -> Gladsheimr
  12 -> Thrymheimr
  13 -> Ydalir

monthSeason :: Month -> Season
monthSeason = \case
  Valaskjolf  -> Winter
  Himinbjorg  -> Winter
  Landvidi    -> Winter
  Sokkvabekkr -> Winter
  Thrudheimr  -> Spring
  Breidablik  -> Spring
  Noatun      -> Spring
  Glitnir     -> Summer
  Folkvangr   -> Summer
  Alfheimr    -> Summer
  Gladsheimr  -> Autumn
  Thrymheimr  -> Autumn
  Ydalir      -> Autumn

monthGod :: Month -> [God]
monthGod = \case
  Valaskjolf  -> [Vali]
  Himinbjorg  -> [Heimdallr, Tyr]
  Landvidi    -> [Vidarr]
  Sokkvabekkr -> [Saga]
  Thrudheimr  -> [Thor]
  Breidablik  -> [Baldr, Jord]
  Noatun      -> [Njordr]
  Glitnir     -> [Forseti]
  Folkvangr   -> [Freyja, Sol]
  Alfheimr    -> [Freyr]
  Gladsheimr  -> [Odinn]
  Thrymheimr  -> [Skadi, Mani]
  Ydalir      -> [Hodr]

seasonAttrMod :: Season -> Attributes -> Attributes
seasonAttrMod season = case season of
  Winter  -> int +~ 1
  Spring  -> str +~ 1
  Summer  -> wil +~ 1
  Autumn  -> con +~ 1
  NewYear -> cha +~ 1
