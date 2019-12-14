module Twarog.Backend.Calendar
  ( Month (..)
  , Day (..)
  , Season (..)
  , Birthday (..)
  -- ** Utils
  , month
  , months
  , monthGod
  , monthSeason
  , seasonAttrMod
  , godsBirthday
  ) where

import Control.Lens
import Data.Map

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
           | Nyarsdagr
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
  Nyarsdagr   -> NewYear

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
  Nyarsdagr   -> [Heimdallr]

-- | Character attribute modifiers according to season of birth.
seasonAttrMod :: Season -> Attributes -> Attributes
seasonAttrMod season = case season of
  Winter  -> int +~ 1
  Spring  -> str +~ 1
  Summer  -> wil +~ 1
  Autumn  -> con +~ 1
  NewYear -> cha +~ 1

-- | Returns god's birthday date.
godsBirthday :: God -> [Birthday]
godsBirthday = \case
  Vali      -> [ Birthday (CommonDay 13) Valaskjolf ]
  Heimdallr -> [ Birthday (CommonDay 13) Himinbjorg
               , Birthday (NewYearsDay) Nyarsdagr
               ]
  Vidarr    -> [ Birthday (CommonDay 13) Landvidi ]
  Saga      -> [ Birthday (CommonDay 13) Sokkvabekkr ]
  Thor      -> [ Birthday (CommonDay 13) Thrudheimr ]
  Baldr     -> [ Birthday (CommonDay 13) Breidablik ]
  Jord      -> [ Birthday (CommonDay 1) Breidablik ]
  Njordr    -> [ Birthday (CommonDay 13) Noatun ]
  Forseti   -> [ Birthday (CommonDay 13) Glitnir ]
  Freyja    -> [ Birthday (CommonDay 13) Folkvangr ]
  Sol       -> [ Birthday (CommonDay 13) Folkvangr ]
  Freyr     -> [ Birthday (CommonDay 13) Alfheimr ]
  Odinn     -> [ Birthday (CommonDay 13) Gladsheimr ]
  Skadi     -> [ Birthday (CommonDay 13) Thrymheimr ]
  Mani      -> [ Birthday (CommonDay 22) Thrymheimr ]
  Hodr      -> [ Birthday (CommonDay 13) Ydalir ]
  _         -> []

birthdayGod :: Birthday -> [God]
birthdayGod = \case
  Birthday (CommonDay 13) Valaskjolf  -> [ Vali ]
  Birthday (CommonDay 13) Himinbjorg  -> [ Heimdallr ]
  Birthday (NewYearsDay) Nyarsdagr    -> [ Heimdallr ]
  Birthday (CommonDay 13) Landvidi    -> [ Vidarr ] 
  Birthday (CommonDay 13) Sokkvabekkr -> [ Saga ]   
  Birthday (CommonDay 13) Thrudheimr  -> [ Thor ]   
  Birthday (CommonDay 13) Breidablik  -> [ Baldr ]  
  Birthday (CommonDay 1) Breidablik   -> [ Jord ]   
  Birthday (CommonDay 13) Noatun      -> [ Njordr ] 
  Birthday (CommonDay 13) Glitnir     -> [ Forseti ]
  Birthday (CommonDay 13) Folkvangr   -> [ Freyja
                                         , Sol ]
  Birthday (CommonDay 13) Alfheimr    -> [ Freyr ]  
  Birthday (CommonDay 13) Gladsheimr  -> [ Odinn ]  
  Birthday (CommonDay 13) Thrymheimr  -> [ Skadi ]  
  Birthday (CommonDay 22) Thrymheimr  -> [ Mani ]   
  Birthday (CommonDay 13) Ydalir      -> [ Hodr ]   
  _ -> []
  
