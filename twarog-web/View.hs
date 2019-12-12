{-# LANGUAGE RecordWildCards #-}
module View (viewModel) where

import           Miso
import           Miso.String
import           Model
import           Twarog.Backend.Character
import           Twarog.Backend.Races
import           Twarog.Backend.Talents
import           Twarog.Backend.Types

import           Control.Lens
import qualified Data.Map                 as M
import           Data.Maybe

viewModel :: Model -> View Msg
viewModel m@Model{..} =
 div_
    [ class_ "" ]
    [ section_
        [ class_ "hero is-medium is-light is-bold", style_  $ M.singleton "padding-top" "2rem" ]
            [ navbarElem m
             , div_ [class_ "hero-body"] [
                div_ [class_ "container has-text-centered"] $
                    [ breadcrumb [NameStage, RaceStage, TalentStage ]]
                    ++ [getStage m]
                ]
            ]
        , div_ [] [text $ ms $ show m]
        , link_
        [ rel_ "stylesheet"
        , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css"
        ]
    ]

getStage :: Model -> View Msg
getStage m = case m ^. currentStage of
                OwnerStage -> askName m
                NameStage -> askName m
              --AtribStage
                RaceStage -> askRace [Just Dwarf, Just Elf, Just Hobgoblin, Just HalfOrc, Just Goblin ] m
 {-               BirthStage
                ArchetypeStage
                GodStage
                SexStage
                HamingjaStage
                FlawStage
                RoleStage
                SkilsStage -}
                TalentStage -> askTalents [ Acrobatic, Aggresive, AnimalFriend, Arachnean, Argonautic, Ascetic ] m

askSample m = div_ [] [text $ ms $ show $ m ^. character . characterName ]

askName :: Model -> View Msg
askName m =
  div_ []
      [ h2_ [class_ "title is-1 has-text-weight-medium"] [ "Your name? "]
        , div_ [class_ "columns has-text-centered"] [
                div_ [ class_ "column is-four-fifths"] [
                            input_ [ class_ "input is-medium"
                                   , value_ $ ms
                                        $ fromMaybe "" (m^.character.characterName)
                                   , onChange Name ]
                        ]
                , div_ [ class_ "column"] [
                            button_ [ class_ "button is-black is-medium"] [ text "Answer" ]
                        ]
                ]
      ]

askRace :: [Maybe Race] -> Model -> View Msg
askRace s m =
  div_ [ class_ "control has-text-centered" ] [
        h2_ [class_ "title is-1 has-text-weight-medium"] [ "Your race? "]
        , div_ [ class_ "columns has-text-centered" ] $ makeRadio m s
        ]

askTalents :: [Talent] -> Model -> View Msg
askTalents s m =
    let talents = fromMaybe [] $ m ^. character . characterTalent
    in
    div_ [ class_ "control has-text-centered" ] [
        div_ [] [
            p_ [class_ "title is-1 is-full has-text-weight-medium"] ["Your talents? "]
            , div_ [ class_ ""]
                [ p_ [class_ "subtitle"] $ ["You can choose "]
                ++ [ text $ ms (maxTalents - Prelude.length talents)]
                ++ [" more talents "]
                ]
            , div_ [ class_ "columns has-text-centered"]
                $ makeCheckbox s m maxTalents
        ]
    ]

makeRadio :: Model -> [Maybe Race] -> [View Msg]
makeRadio m = 
    let actualRace = m ^. character . characterRace 
    in
    Prelude.map
        (\x ->
            div_ [class_ "column has-text-centered"]
                [label_ [ class_ "radio is-size-5"]
                    [input_
                        [type_ "radio", name_ "race", onChecked $ RaceChecked x
                        , checked_ (x == actualRace)
                        , style_  $ M.singleton "margin" "0.5rem"]
                    , text $ ms $ case x of 
                                    Just a -> show a
                                    Nothing -> ""
                    ]
                ]
        )

makeCheckbox :: [Talent] -> Model -> Int -> [View Msg]
makeCheckbox t m max = let talents = fromMaybe [] $ m ^. character . characterTalent
                        in
                     Prelude.map ( \x ->
                    div_ [ class_ "column has-text-centered" ] [
                        label_ [ class_ "checkbox radio is-size-5" ] [
                            input_ [ type_ "checkbox", name_ "talent", style_  $ M.singleton "margin" "0.5rem"
                                    , checked_ $ elem x talents
                                    , disabled_ ( Prelude.length talents >= max
                                                && notElem x talents )
                                    , onChecked $ TalentChecked x ]
                            , text $ ms $ show x
                        ]
                        ])
                  t

navbarElem m =
    let
        getAtr f = case m ^. character . characterAttr of
                        Just a -> a ^. f
                        Nothing -> 0
        charisma = getAtr cha
        costitution =  getAtr con
        dexterity = getAtr dex
        inteligence = getAtr int
        strength = getAtr str
        will = getAtr wil
    in
    nav_ [class_ "level is-mobile"][
        div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Charisma"]
                , p_ [class_ "title"] [ text $ ms $ show charisma
                                        ]
            ]
        ]
        , div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Constitution"]
                , p_ [class_ "title"] [ text $ ms $ show costitution ]
            ]
        ]
        , div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Dexterity"]
                , p_ [class_ "title"] [ text $ ms $ show dexterity ]
            ]
        ]
        , div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Inteligence"]
                , p_ [class_ "title"] [ text $ ms $ show inteligence ]
            ]
        ]
        , div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Strength"]
                , p_ [class_ "title"] [ text $ ms $ show strength ]
            ]
        ]
        , div_ [class_ "level-item has-text-centered"] [
            div_ [] [
                p_ [class_ "heading"] [ "Will"]
                , p_ [class_ "title"] [ text $ ms $ show will ]
            ]
        ]
    ]

breadcrumb :: [Stage] -> View Msg
breadcrumb stages = nav_ [class_ "breadcrumb is-centered"] [
                ul_ [] $ Prelude.map
                        ( \x -> li_ [] [
                            a_ [ onClick (ChangeStage x) ] [ text $ ms $ show x ]
                        ])
                        stages
                ]