{-# LANGUAGE FlexibleInstances #-}
module Model
    ( Model(..)
    , character
    , currentStage
    , currentAtribBounce
    , currentRoll1
    , currentRoll2
    , Msg (..)
    , Stage (..)
    , initialModel
    , maxTalents
    , maxFlaws
    , maxAtrValue
    ) where

import           Control.Lens
import           Miso
import           Miso.String
import           Twarog.Backend.Archetypes
import           Twarog.Backend.Calendar
import           Twarog.Backend.Character
import           Twarog.Backend.Flaws
import           Twarog.Backend.Gods
import           Twarog.Backend.Item
import           Twarog.Backend.Races
import           Twarog.Backend.Skills
import           Twarog.Backend.Talents
import           Twarog.Backend.Types

data Stage = OwnerStage | NameStage | AtribStage Int | RaceStage | BirthStage
            | ArchetypeStage | GodStage | SexStage | HamingjaStage
            | FlawStage | RoleStage | SkilsStage | TalentStage
            deriving (Show, Eq)

data Model = Model
    { _currentStage :: Stage
    , _currentAtribBounce :: Int
    , _currentRoll1 :: Int
    , _currentRoll2 :: Int
    , _character    :: NewCharacter
    } deriving (Show, Eq)
makeLenses ''Model

data Msg =  Name MisoString
            | Talents [String]
            | RaceChecked (Maybe Race) Checked
            | TalentChecked Talent Checked
            | FlawChecked Flaw Checked
            | NoOp
            | AskName
            | AskRace
            | AskTalents
            | ChangeStage Stage
            | SetAttribute Int Int
            | SetCurrentRoll1 MisoString
            | SetCurrentRoll2 MisoString
            deriving (Show, Eq)

maxTalents :: Int
maxTalents = 3

maxFlaws :: Int
maxFlaws = 3

maxAtrValue :: Int
maxAtrValue = 18

initialModel :: Model
initialModel = Model (AtribStage 1) 0 0 0
                $ NewCharacter Nothing Nothing Nothing
                                Nothing Nothing Nothing
                                Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing