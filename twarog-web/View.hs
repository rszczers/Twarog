module View (viewModel) where

import       Miso
import       Miso.String
import       Model
import       Twarog.Backend.Character
import       Twarog.Backend.Races
import       Twarog.Backend.Talents
import       Twarog.Backend.Types
import       Twarog.Backend.Flaws
import       Twarog.Backend.Calendar
import       Twarog.Frontend.DiceGen

import       Control.Lens
import qualified Data.Map         as M
import       Data.Maybe
import       Data.Typeable

viewModel :: Model -> View Msg
viewModel m@Model{..} =
 div_
  [ class_ "" ]
  [ section_
    [ class_ "hero is-medium is-light is-bold", style_  $ M.singleton "padding-top" "2rem" ]
      [ navbarElem m
       , div_ [class_ "hero-body"] [
        div_ [class_ "container has-text-centered"] $
          [ breadcrumb [NameStage, RaceStage, TalentStage, (AttribStage Nothing), FlawStage, BirthStage, SexStage]]
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
  ]

getStage :: Model -> View Msg
getStage m = case m ^. currentStage of
        OwnerStage -> askName m
        NameStage -> askName m
        AttribStage n -> askAttributes m n
        RaceStage -> displayRadioQuestion (Prelude.map Just races) m
                  characterRace "Your race?" RaceChecked 
        BirthStage -> displayBirthday m
      {-  ArchetypeStage
        GodStage -}
        SexStage -> displayRadioQuestion (Prelude.map Just sexes) m 
                        characterSex "Your sex?" SexChecked
      --  HamingjaStage 
        FlawStage -> displayCheckboxQuestion [ Alcoholic FlawLevel1
                    , Annoying FlawLevel1
                    , BadBack FlawLevel1
                    , BadSight FlawLevel1
                    , BadTempered FlawLevel1
                    , ChronicPain FlawLevel1] m characterFlaws "What are your flaws?" Nothing FlawChecked
      {-  RoleStage
        SkilsStage -}
        TalentStage -> displayCheckboxQuestion [ Acrobatic, Aggresive, AnimalFriend, Arachnean, Argonautic, Ascetic ] m
                            characterTalent "What are your talents?" (Just maxTalents) TalentChecked

askName :: Model -> View Msg
askName m =
  div_ []
    [ h2_ [class_ "title is-1 has-text-weight-medium"] [ "Your name? "]
    , div_ [class_ "columns has-text-centered"] [
        div_ [ class_ "column is-four-fifths"] [
            input_ [ class_ "input is-medium"
              , value_ $ ms
                $ fromMaybe "" (m^.character.characterName)
              , onChange Name 
              ]
          ]
        , div_ [ class_ "column"] [
              button_ [ class_ "button is-black is-medium"] [ text "Answer" ]
          ]
        ]
    ]

displayBirthday m =
  let
    birthday = m ^. character . characterBirth
    day =  case birthday of
          Nothing -> ""
          Just a -> show $ a ^. birthdayDay
    month = case birthday of
          Nothing -> ""
          Just a -> show $ a ^. birthdayMonth
  in
    div_ [] [ 
      h2_ [class_ "title is-1 has-text-weight-medium"] [ "Your birthday? "]
      , div_ [class_ "columns"] [
        div_ [ class_ "column field"] [
          label_ [class_ "label"] ["Day"]
          , p_ [] [text $ ms $ day]
          ]
        , div_ [class_ "column field"] [
          label_ [class_ "label"] ["Month"]
          , p_ [] [text $ ms $ month]
          ]
        ]
      , div_ [class_ ""] [
        button_ [class_ "button is-medium is-black"
            , onClick SetRandomBirth ] 
          ["Generate birthday"]
        ]
    ]
      

--dispaleyCheckboxQuestion :: [a] -> Model -> (characterField) -> String -> Int -> (a -> Bool -> View Msg)
displayCheckboxQuestion valueList model characterField question max msg =
  let 
    content = fromMaybe [] $ model ^. character . characterField
    isDisabled x =  case max of 
              Just a -> Prelude.length content >= a
                  && notElem x content 
              Nothing -> False

  in
    div_ [ class_ "control has-text-centered" ] [
      div_ [] [
        p_ [class_ "title is-1 is-full has-text-weight-medium"] [ question ]
        , div_ [ class_ ""]
          (
            case max of
              Just a -> 
                [ p_ [class_ "subtitle"] 
                    $ ["You can choose "]
                    ++ [ text $ ms (a - Prelude.length content)]
                    ++ [" more "]
                ]
              Nothing -> []
          )
        , div_ [ class_ "columns has-text-centered is-multiline is-mobile"]
          $ Prelude.map 
            ( \x ->
              label_ [class_ "label has-text-weight-normal"] [
                div_ [class_ "field has-addons column has-text-centered is-one-fifth-tablet is-one-quarters-mobile"] [ 
                  p_ [class_ "control"] [
                    input_ [ 
                      type_ "checkbox", name_ "talent"
                      , style_  $ M.singleton "margin" "0.5rem"
                      , checked_ $ elem x content
                      , disabled_ $ isDisabled x
                      , onChecked $ msg x 
                    ]
                    ]
                  , p_ [class_ "control"] [ 
                    text $ ms $ show x
                    ]
                  ]
                ]
            ) valueList
      ]
    ]
   
--dispaleyRadioQuestion :: [a] -> Model -> (characterField) -> String ->  (a -> Bool -> View Msg)
displayRadioQuestion valueList model characterField question msg =
  let 
    content = model ^. character . characterField 
  in
    div_ [ class_ "control has-text-centered" ] [
      h2_ [class_ "title is-1 has-text-weight-medium"] [ question ]
      , div_ [ class_ "columns has-text-centered is-multiline is-mobile" ]
      $ Prelude.map
        (\x ->
          label_ [class_ "label has-text-weight-normal"] [
            div_ [class_ "field has-addons column has-text-centered is-one-fifth-tablet is-one-quarters-mobile"] [
                p_ [class_ "control"] [
                input_ [
                  type_ "radio", name_ "race", onChecked $ msg x
                  , checked_ (x == content)
                  , style_  $ M.singleton "margin" "0.5rem"
                  ]
                  ]
                ,p_ [class_ "control"] [
                  text $ ms $ case x of 
                        Just a -> show a
                        Nothing -> ""
                ]
              ]
            ]
        ) valueList
    ]

attribToBounce :: Model -> Maybe AttribBounce
attribToBounce m = m ^. currentAttribBounce

navbarElem m =
  let
    getAttr f = case m ^. character . characterAttr of
            Just a -> a ^. f
            Nothing -> 0
    charisma = getAttr cha
    costitution =  getAttr con
    dexterity = getAttr dex
    inteligence = getAttr int
    strength = getAttr str
    will = getAttr wil

    printAttribute attr bounce = 
      div_ [class_ "level-item has-text-centered"] [
        div_ [ class_ 
          (if attribToBounce m == bounce
          then "animated bounce"
          else "")
          ] [
            p_ [class_ "heading"] [ text $ ms $ printBounce $ bounce]
            , p_ [class_ "title"] [ text $ ms $ show attr]
          ]
      ]
  in
    nav_ [class_ $ ms $ "level is-mobile " ++ 
              (if (attribToBounce m == (Just Every)) 
              then "animated bounce"
              else "")
      ] [
      printAttribute charisma $ Just Charisma
      ,  printAttribute costitution $ Just Constitution
      ,  printAttribute dexterity $ Just Dexterity
      ,  printAttribute inteligence $ Just Inteligence
      ,  printAttribute strength $ Just Strength
      ,  printAttribute will $ Just WillPower
    ]

breadcrumb :: [Stage] -> View Msg
breadcrumb stages = 
  nav_ [class_ "breadcrumb is-centered"] [
    ul_ [] $ Prelude.map
        ( \x -> li_ [] [
          a_ [ onClick (ChangeStage x) ] [ text $ ms $ show x ]
        ])
        stages
    ]


askAttributes :: Model -> Maybe AttribBounce -> View Msg
askAttributes m bounce = 
  let 
    firstScreen = isNothing bounce
    attr = printBounce bounce
    max3D6 = 18
    isNotValid = not $ (Prelude.foldr max max3D6 [m ^. currentRoll1, m ^. currentRoll2]) == max3D6
  in  
    if firstScreen 
    then attributesFirstScreen
    else   
      div_ [] [
        p_ [class_ "title is-1 has-text-weight-medium"] [text "Roll your dice!"]
        , p_ [class_ "subtitle is-3"] [text $ ms $ "Determine your " ++ attr ]
        , p_ [class_ "subtitle"] [text "Three d6 two times" ]
        , div_ [class_ "columns is-centered"
            --, style_  $ M.singleton "margin" "0.5rem" 
          ] [
            div_ [class_ "column is-one-fifth"] [
              input_ [class_ "input is-medium "
                --, style_  $ M.singleton "margin-right" "0.5rem"
                , type_ "number"
                , max_ $ ms $ show $ max3D6
                , value_ (ms $ show (m ^. currentRoll1))
                , onInput SetCurrentRoll1 ]
            ]
            , div_ [class_ "column  is-one-fifth"] [
              input_ [class_ "input is-medium "
                --, style_  $ M.singleton "margin-right" "0.5rem"
                , type_ "number"
                , max_ $ ms $ show $ max3D6
                , value_ (ms $ show $ m ^. currentRoll2)
                , onInput SetCurrentRoll2 ]
            ]
        ]
        , div_ [] 
            (if isNotValid
            then 
              [
                article_ [class_ "message is-danger"] [
                  div_ [class_ "messege-body"] [
                    text $ ms $ "Wait a sec are u trying to cheat me. "
                    ++ "Maximum of 3xD6 roll is 18. "
                  ]
                ]
              ]
              else [])
        , button_ [ class_ "button is-black is-large"
              , style_  $ M.singleton "margin-top" "1rem"
              , disabled_ isNotValid
              , onClick (SetAttribute (
                    max (m ^. currentRoll1) (m ^. currentRoll2))
                    bounce )
              ] 
              [text "Calculate"]
        ]

attributesFirstScreen :: View Msg
attributesFirstScreen = div_ [] [
              p_ [class_ "title is-1 has-text-weight-medium"] [text "Your attributes"] 
              , div_ [class_ "columns "] [
                div_ [class_ "level is-mobile column is-three-fifths is-offset-one-fifth"] [
                  button_ [class_ "level-item button is-large is-black"
                      , onClick $ ChangeStage $ AttribStage $ Just Charisma
                      ] ["Use your dices"]   
                  , label_ [class_ "level-item label is-medium"] [" or "]
                  , button_ [class_ "level-item button is-large is-black"
                         , onClick SetRandomAttr               -- Tu bym chciała dać atrybuty      
                      ] ["Use one click generator"]
                ]  
              ]
            ] 
