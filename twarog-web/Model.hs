module Model
  ( Model(..)
  , character
  , currentStage
  , currentAttribBounce
  , currentRoll1
  , currentRoll2
  , Msg (..)
  , Stage (..)
  , AttribBounce (..)
  , printBounce
  , initialModel
  , maxTalents
  ) where

import       Control.Lens
import       Miso
import       Miso.String
import       Data.Char as C
import       Twarog.Backend.Archetypes
import       Twarog.Backend.Calendar
import       Twarog.Backend.Character
import       Twarog.Backend.Flaws
import       Twarog.Backend.Gods
import       Twarog.Backend.Item
import       Twarog.Backend.Races
import       Twarog.Backend.Skills
import       Twarog.Backend.Talents
import       Twarog.Backend.Types

data Stage = OwnerStage | NameStage | AttribStage (Maybe AttribBounce) | RaceStage | BirthStage
      | ArchetypeStage | GodStage | SexStage | HamingjaStage
      | FlawStage | RoleStage | SkilsStage | TalentStage
      deriving (Show, Eq)

data AttribBounce = Every | Charisma | Constitution | Dexterity | Inteligence | Strength | WillPower 
      deriving (Show, Eq)

printBounce :: Maybe AttribBounce -> String
printBounce b = 
  case b of
    Just a -> Prelude.map C.toLower $ show a
    Nothing -> ""
  
data Model = Model
  { _currentStage :: Stage
  , _currentAttribBounce :: Maybe AttribBounce
  , _currentRoll1 :: Int
  , _currentRoll2 :: Int
  , _character  :: NewCharacter
  } deriving (Show, Eq)
makeLenses ''Model

data Msg =  Name MisoString
      | Talents [String]
      | RaceChecked (Maybe Race) Checked
      | TalentChecked Talent Checked
      | FlawChecked Flaw Checked
      | SexChecked (Maybe Sex) Checked
      | NoOp
      | AskName
      | AskRace
      | AskTalents
      | ChangeStage Stage
      | SetAttribute Int (Maybe AttribBounce)
      | SetAllAttributes Attributes
      | SetCurrentRoll1 MisoString
      | SetCurrentRoll2 MisoString
      | SetAttrBounce AttribBounce
      | SetRandomAttr
      | SetBirth Birthday
      | SetRandomBirth
      deriving (Show, Eq)

maxTalents :: Int
maxTalents = 3

initialModel :: Model
initialModel = Model (AttribStage Nothing) Nothing 0 0
        $ NewCharacter Nothing Nothing Nothing
                Nothing Nothing Nothing
                Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing
