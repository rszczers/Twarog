module View.Race (askRace) where

import       Miso
import       Miso.String
import       Model
import       Twarog
import       Control.Lens
import       Data.Maybe

import      View.Buttons

askRace :: Model -> View Msg
askRace m =
  div_ [class_ "animated fadeIn"] [
    displayRadioQuestion (Prelude.map Just playableRaces) m
                  characterRace "Your race?" RaceChecked 
    , previewReace $ m ^. character 
    , chooseRandomlyButton SetRandomRace
    , nextButton RaceStage $ isNothing $ m ^. character . characterRace
  ]

previewReace :: NewCharacter -> View Msg
previewReace ch = 
  div_ [class_ "columns is-centered"] [
    div_ [class_ "column is-10 content has-text-left"] [
      ul_ [] $
        Prelude.map 
          (\x -> li_ [] [ text $ ms $ show x ])
          $ (addMod ch (fromMaybe Elf (ch ^. characterRace))) ^. characterOther 
      ]
  ]