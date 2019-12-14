module Twarog.Backend.Calendar
  ( Month (..)
  , Day (..)
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

data Day where
  CommonDay :: Int -> Day
  NewYearsDay :: Day
  deriving (Eq, Show) 

instance Bounded Day where
  minBound = CommonDay 1
  maxBound = CommonDay 28

data Season = NewYear
            | Winter
            | Spring
            | Summer
            | Autumn
            deriving (Eq, Show, Enum)

data Birthday =
  Birthday { birthdayDay :: Day
           , birthdayMonth :: Month
           } deriving (Eq, Show)

-- | Get list of months.
months :: [Month]
months = enumFrom (toEnum 0)

-- | Convert Int to @Month@.
month :: Int -> Month
month x = if x > 0 
          then toEnum $ x - 1
          else error "No such month"

-- | Get month according to month name.
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

-- | Get list of divine's birthdays for given month.
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

-- | Character attribute modifiers according to season of birth.
seasonAttrMod   :: Season -> Attributes -> Attributes
seasonAttrMod   season = case season of
  Winter  -> int +~ 1
  Spring  -> str +~ 1
  Summer  -> wil +~ 1
  Autumn  -> con +~ 1
  NewYear -> cha +~ 1
                  
