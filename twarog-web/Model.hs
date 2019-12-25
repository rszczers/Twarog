module Model
  ( Model(..)
  , character
  , currentStage
  , availableStages
  , currentAttribBounce
  , currentRoll1
  , currentRoll2
  , Msg (..)
  , Stage (..)
  , AttribBounce (..)
  , MaxCheckbox (..)
  , printBounce
  , initialModel
  , nextStage
  ) where

import       Control.Lens
import       Miso
import       Miso.String
import       Data.Char as C
import       Twarog

data Stage = OwnerStage 
           | NameStage 
           | AttribStage (Maybe AttribBounce) 
           | RaceStage 
           | BirthStage
           | ArchetypeStage 
           | AttitudeStage 
           | GodStage 
           | SexStage 
           | HamingjaStage 
           | FlawsAndTalentsStage
           | RoleStage | SkilsStage 
           deriving (Show, Eq)

data AttribBounce = Every 
                  | Charisma 
                  | Constitution 
                  | Dexterity 
                  | Inteligence 
                  | Strength 
                  | WillPower 
                  deriving (Show, Eq)

nextStage :: Stage -> Stage
nextStage s = case s of 
  NameStage            -> AttribStage Nothing
  AttribStage _        -> BirthStage
  BirthStage           -> SexStage
  SexStage             -> RaceStage
  RaceStage            -> AttitudeStage
  AttitudeStage        -> ArchetypeStage 
  ArchetypeStage       -> FlawsAndTalentsStage
  RoleStage            -> SkilsStage
  FlawsAndTalentsStage -> RoleStage

printBounce :: Maybe AttribBounce -> String
printBounce b = case b of
  Just a -> C.toLower <$> show a
  Nothing -> ""
  
data Model = Model
  { _currentStage :: Stage
  , _availableStages :: [Stage]
  , _currentAttribBounce :: Maybe AttribBounce
  , _currentRoll1 :: Int
  , _currentRoll2 :: Int
  , _character  :: NewCharacter
  } deriving (Show, Eq)
makeLenses ''Model

data Msg =  Name MisoString
      -- Checkbox msgs
      | TalentChecked Talent (Maybe Int) Checked
      | FlawChecked Flaw (Maybe Int) Checked
      -- Radiobox msgs
      | SexChecked (Maybe Sex) Checked
      | RaceChecked (Maybe Race) Checked
      -- Character related msgs
      | AskName
      | AskRace
      | AskTalents
      | SetAttribute Int (Maybe AttribBounce)
      | SetAllAttributes Attributes
      | SetRandomAttr
      | SetBirth Birthday
      | SetRandomBirth
      -- No character related msgs
      | NoOp
      | ChangeStage Stage
      | AddAvailableStage Stage
      | SetCurrentRoll1 MisoString
      | SetCurrentRoll2 MisoString
      | SetAttrBounce AttribBounce
      deriving (Show, Eq)

data MaxCheckbox = TalentsMax Int | NoLimit

initialModel :: Model
initialModel =
  let _currentStage = NameStage 
      _currentAttribBounce = Nothing 
      _availableStages = []
      _currentRoll1 = 0
      _currentRoll2 = 0
      _character = emptyNewCharacter
   in Model{..}

