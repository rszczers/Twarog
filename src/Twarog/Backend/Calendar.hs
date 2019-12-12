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
           deriving (Ord, Eq, Show, Enum)

data Season = NewYear
            | Winter
            | Spring
            | Summer
            | Autumn
            deriving (Eq, Show, Enum)

month :: Int -> Month
month x = if x > 0 
          then toEnum $ x - 1
          else error "No such month"

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
