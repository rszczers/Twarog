module View (viewModel) where

import       Miso
import       Miso.String
import       Model
import       Twarog
import       Data.Text as T

import       Control.Lens
import       Data.Maybe
import qualified Data.Set         as S
import qualified Data.Map         as M

import      View.Attributes
import      View.Birthday 
import      View.Buttons
import      View.Attitude
import      View.FlawsAndTalents
import      View.Skills
import      View.Menu
import      View.Character
import      View.Race

viewModel :: Model -> View Msg
viewModel m@Model{..} =
 div_
  [ class_ "" ]
  [ section_
    [ class_ "hero is-medium is-light is-bold", style_  $ M.singleton "padding-top" "2rem" ]
      [ navbarElem m
       , menu
       , div_ [class_ "hero-body"] [
          div_ [class_ "container has-text-centered"] $
            [ breadcrumb (m ^. availableStages) (m ^. currentStage) ]
            ++ [getStage m]
          ]
      ]
    , div_ [] [text $ ms $ show m]
    , link_
    [ rel_ "stylesheet"
    , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css"
    ]
    , link_ 
    [ rel_ "stylesheet"
    , href_ "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.7.2/animate.min.css"
    ]
    , script_ [defer_ "True", src_ "https://use.fontawesome.com/releases/v5.4.2/js/all.js"] []
  ]

getStage :: Model -> View Msg
getStage m = case m ^. currentStage of
        OwnerStage                  -> askName m
        NameStage                   -> askName m
        AttribStage n               -> askAttributes m n
        RaceStage                   -> askRace m
        BirthStage                  -> askBirthday m
        AttitudeStage               -> askAttitude m
        GodStage                    -> askLifeStance m 
        SexStage                    -> askSex m
      --  HamingjaStage 
        FlawsAndTalentsStage False  -> flawsAndTalentsFirstScreen m
        FlawsAndTalentsStage True   -> askFlawsAndTalents m
        RoleStage                   -> askRoles m
        RandomSkillsStage           -> skillsFirstScreen m
        SkillsStage                 -> askSkills m
        SummaryStage                -> characterSummary m
        GenCharacterStage           -> genCharacterScreen m 

askName :: Model -> View Msg
askName m =
  let 
    name = m ^. character . characterName
    nameSugestion = m ^. possibleNames
  in
    div_ [class_ "animated fadeIn"] [ 
      h2_ [class_ "title is-2 has-text-weight-medium"] [ "Your name? "]
      , div_ [class_ "columns is-centered"] [
          div_ [ class_ "column is-two-fifths"] [
            input_ [ class_ "input is-medium"
            , value_ $ ms
                $ fromMaybe "" name
            , onInput $ SuggestRandomNames
            ]
          ]
        ]
      , div_ [class_ "columns ", onCreated $ SuggestRandomNames "" ] [
        div_ [class_ "column animated bounceInLeft"] [
          h4_ [class_ "title"] ["Consider choosing…"]
          , div_ [class_ "tags"] $ 
              Prelude.map  
                (\(n, s, r) ->
                  case s of
                    Female ->  
                      a_ [onClick $ Name (ms n)] [
                        span_ [class_ "tag is-danger is-light is-medium"
                              , style_ $ M.singleton "margin" "0.1rem"] [
                        text $ ms $ n
                        ]
                      ]
                    Male -> 
                      a_ [onClick $ Name (ms n)] [
                        span_ [class_ "tag is-primary is-light is-medium"
                              , style_ $ M.singleton "margin" "0.1rem"] [
                        text $ ms $ n
                        ]
                      ]
                    Non -> text ""

                ) 
                nameSugestion
          ]
        ] 
        , nextButton NameStage (name ==  Nothing || name == Just "")
      ]

askSex :: Model -> View Msg
askSex m = 
  div_ [class_ "animated fadeIn"] [    
    displayRadioQuestion (Prelude.map Just sexes) m 
                        characterSex "Your sex?" SexChecked
    , chooseRandomlyButton SetRandomSex
    , nextButton SexStage $ isNothing $ m ^. character . characterSex
    ]

askLifeStance :: Model -> View Msg
askLifeStance m = 
  let 
    race = m ^. character . characterRace
    valueList = case race of
                  Just r -> if (isRaceTraditionalOnly r) then [Just Traditional] else [Just Traditional] 
                    ++ Prelude.map (\g -> Just $ Religious g ) gods
                  Nothing -> []
  in
    div_ [] [
     displayRadioQuestion valueList m characterLifeStance "Your life stance?" LifeStanceChecked 
     , chooseRandomlyButton SetRandomLifeStance
     , nextButton GodStage $ isNothing $ m ^. character . characterLifeStance
    ]

askRoles :: Model -> View Msg
askRoles m =
  let
      -- Temporary solution
      attr = fromMaybe (Attributes 0 0 0 0 0 0) $ m ^. character . characterAttr
      talents = S.toList $ m ^. character . characterTalent
      lifeStance = fromMaybe Traditional $ m ^. character . characterLifeStance
      race = fromMaybe Elf $ m ^. character . characterRace
      sex = fromMaybe Non $ m ^. character . characterSex
      archetype = fromMaybe Athenic $ m ^. character . characterAlignment
  in
    div_ [] [
      displayRadioQuestion 
          (Prelude.map Just (availableRoles attr talents lifeStance race sex archetype))
          m characterRole "Your role?" RoleChecked
      , chooseRandomlyButton SetRandomRole
      , nextButton RoleStage $ isNothing $ m ^. character . characterRole
    ]





