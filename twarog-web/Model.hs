module Model
    ( Model(..)
    , character
    , currentStage
    , Msg (..)
    , Stage (..)
    , initialModel
    , maxTalents

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

data Stage = OwnerStage | NameStage | AtribStage | RaceStage | BirthStage
            | ArchetypeStage | GodStage | SexStage | HamingjaStage
            | FlawStage | RoleStage | SkilsStage | TalentStage
            deriving (Show, Eq)

data Model = Model
    { _currentStage :: Stage
    , _character    :: NewCharacter
    } deriving (Show, Eq)
makeLenses ''Model

data Msg =  Name MisoString
            | Talents [String]
            | RaceChecked (Maybe Race) Checked
            | TalentChecked Talent Checked
            | UpdateField MisoString
            | SayHelloWorld
            | NoOp
            | AskName
            | AskRace
            | AskTalents
            | ChangeStage Stage
            deriving (Show, Eq)

maxTalents :: Int
maxTalents = 3

initialModel :: Model
initialModel = Model NameStage
                $ NewCharacter Nothing Nothing Nothing
                                Nothing Nothing Nothing
                                Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing