module Twarog.Backend.Calendar
  ( Month (..)
  , Season (..)
  , month
  , monthGod
  , seasonAttrMod
  ) where

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
seasonAttrMod season attr = case season of
  Winter -> attr ^. int +~ 1
  Spring -> attr ^. str +~ 1
  Summer -> attr ^. wil +~ 1
  Autumn -> attr ^. con +~ 1
  NewYear -> attr ^. cha +~ 1
