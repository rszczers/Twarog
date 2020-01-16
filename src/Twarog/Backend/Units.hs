{- |
This module is inspired in many ways by
https://kowainik.github.io/posts/membrain
-}

module Twarog.Backend.Units 
  (
  -- * Weight
  -- ** Units
    Weight (..)
  , Miligram
  , Gram
  , Kilogram
  , Carat
  , Ort
  , Sicilium
  , Ounce 
  , Mark
  , BowlPound
  , WeightPound
  -- ** Conversions 
  , gram
  , kilogram
  , carat
  , ort
  , sicilium
  , ounce 
  , mark
  , bowlPound
  , weightPound
  -- * Distance 
  , Distance (..)
  -- ** Units
  , Micrometre
  , Milimetre
  , Centimetre
  , Scruple
  , Line
  , Thumb
  , Inch
  , Palm
  , Foot
  , Ell
  , Step
  , Yard
  , Pace
  , Fathom
  , Rod
  , StoneThrow
  , ArrowShoot
  , Furlong
  , Mile
  , Quarter
  , Rest
  , Road
  -- ** Conversions 
  , distance
  , toDistance
  , milimetre
  , centimetre
  , scruple
  , line
  , thumb
  , inch
  , palm
  , foot
  , ell
  , step
  , yard
  , pace
  , fathom
  , rod
  , stoneThrow
  , arrowShoot
  , furlong
  , mile
  , quarter
  , rest
  , road
  -- * Capacity
  , Capacity
  -- ** Units
  , Microlitre
  , Dose
  , Pel
  , Just
  , Pot
  , Can
  , Bowl
  , Barrel
  , Cargo
  -- ** Conversions
  , microlitre
  , dose
  , pel
  , just
  , pot
  , can
  , bowl
  , barrel
  , cargo
  -- * Time
  , Time
  -- ** Units
  , Glass
  , Hour
  , Shift
  -- ** Conversions
  , glass
  , hour
  , shift
  , day
  -- * Value
  , Value
  -- ** Units
  , Lead
  , Iron
  , Copper
  , Salt
  , Bronze
  , Silver
  , Aurichalcum
  , Electrum
  , Gold
  -- ** Conversions
  , lead
  , iron
  , copper
  , salt
  , bronze
  , silver
  , aurichalcum
  , electrum
  , gold
  ) where

import Twarog.Backend.Units.Capacity
import Twarog.Backend.Units.Distance
import Twarog.Backend.Units.Time
import Twarog.Backend.Units.Value
import Twarog.Backend.Units.Weight