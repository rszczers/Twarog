module View (viewModel) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import qualified Data.Map         as M
import       Data.Maybe
import       Data.Typeable
import qualified Data.Set         as S

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
    , script_ [defer_ "True", src_ "https://use.fontawesome.com/releases/v5.3.1/js/all.js"] []
  ]

getStage :: Model -> View Msg
getStage m = case m ^. currentStage of
        OwnerStage -> askName m
        NameStage -> askName m
        AttribStage n -> askAttributes m n
        RaceStage -> askRace m
        BirthStage -> askBirthday m
        AttitudeStage -> askAttitude m
        --GodStage 
        SexStage -> askSex m
      --  HamingjaStage 
        FlawsAndTalentsStage -> askFlawsAndTalents m
      {-  RoleStage
        SkilsStage -}

nextButton :: Stage -> String -> View Msg                            
nextButton stage txt = 
  div_ [class_ "columns"][
    div_ [class_ "column is-full level"] [
      div_ [ class_ "level-right"] [
        button_ [ class_ "button is-outlined is-medium"
                , onClick $ ChangeStage $ nextStage stage  ] 
                [ 
                  text $ ms $ txt
                  , span_ [class_ "icon", style_ $ M.singleton "padding-left" "1.5rem"] [
                      i_ [class_ "fas fa-chevron-right"] []
                    ]
                 ]
        ]
      ]
    ]
    
askName :: Model -> View Msg
askName m =
  div_ [class_ "animated fadeIn"]
    [ h2_ [class_ "title is-2 has-text-weight-medium"] [ "Your name? "]
    , div_ [class_ "columns is-centered"] [
        div_ [ class_ "column is-two-fifths"] [
            input_ [ class_ "input is-medium"
              , value_ $ ms
                $ fromMaybe "" (m^.character.characterName)
              , onInput Name 
              ]
          ]
        ]
        , nextButton NameStage "Go to attributes"
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
      div_ [class_ "animated fadeIn"] [
        p_ [class_ "title is-2 has-text-weight-medium"] [text "Roll your dice!"]
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
        , div_ [class_ "columns is-centered"] 
            (if isNotValid
            then 
              [
                article_ [class_ "message is-danger animated bounceInDown "] [
                  div_ [class_ "message-header"] [
                    p_ [] ["Maximum of 3xD6 roll is 18. "]
                  ]
                  , div_ [class_ "message-body"] [
                      figure_ [] [
                        img_ [src_ "https://i.kym-cdn.com/entries/icons/original/000/005/540/130221984383.png"]
                      ]
                    ]
                  ]
                  ]
              else [])
        , button_ [ class_ "button is-black is-medium"
              , style_  $ M.singleton "margin-top" "1rem"
              , disabled_ isNotValid
              , onClick (SetAttribute (
                    max (m ^. currentRoll1) (m ^. currentRoll2))
                    bounce )
              ] 
              [text "Calculate"]
        ]

attributesFirstScreen :: View Msg
attributesFirstScreen = div_ [class_ "animated fadeIn"] [
              p_ [class_ "title is-2 has-text-weight-medium"] [text "Your attributes"] 
              , div_ [class_ "columns "] [
                div_ [class_ "level is-mobile column is-three-fifths is-offset-one-fifth"] [
                  button_ [class_ "level-item button is-medium is-black"
                      , onClick $ ChangeStage $ AttribStage $ Just Charisma
                      ] ["Use your dices"]   
                  , label_ [class_ "level-item label is-medium"] [" or "]
                  , button_ [class_ "level-item button is-medium is-black"
                         , onClick SetRandomAttr 
                      ] ["Use one click generator"]
                ]  
              ]
              , nextButton (AttribStage Nothing) "Go to Birthday"
            ] 

askBirthday m =
  let
    birthday = m ^. character . characterBirth
    day =  case birthday of
          Nothing -> ""
          Just a -> show $ a ^. birthdayDay
    month = case birthday of
          Nothing -> ""
          Just a -> show $ a ^. birthdayMonth
    god = case birthday of
          Just b -> birthdayGod b
          Nothing -> []
  in
    div_ [class_ "animated fadeIn"] [
      h2_ [class_ "title is-2 has-text-weight-medium"] [ "Your birthday? "]
    , div_ [class_ "columns is-mobile is-multiline is-centered"] [ 
            div_ [ class_ "column is-one-fifth field"] [
              label_ [class_ "label"] ["Day"]
              , p_ [] [text $ ms $ day]
              ]
            , div_ [class_ "column is-one-fifth field"] [
              label_ [class_ "label"] ["Month"]
              , p_ [] [text $ ms $ month]
              ]
          ]
      , div_ [class_ "columns is-centered"] [
          div_ [class_ "column is-half"] [
          button_ [class_ "button is-medium is-black"
              , onClick SetRandomBirth ] 
            ["Generate birthday"]
          ]
      ]
      , div_ [class_ "columns is-centered"] [
          article_ 
            [ class_ 
                (if god /= [] 
                then " message is-success animated fadeInDown" 
                else "")
            ]
            (case god of
              [ a ] -> 
                [ div_ [class_ "message-header"] [
                    p_ [] ["You are marked!"]    
                  ]
                , div_ [class_ "message-body",
                      style_  $ M.singleton "padding" "0.5rem"] [
                  text $ ms $ "This is " ++ show a ++ " day. You obtain additional talent: Marked"
                  ]
                ]
              _ -> [ text $ ""]  
            )
        ]
        , nextButton BirthStage "Go to Sex"
    ]
    
askSex m = 
  div_ [class_ "animated fadeIn"] [    
    displayRadioQuestion (Prelude.map Just sexes) m 
                        characterSex "Your sex?" SexChecked
    , nextButton SexStage "Go to Race"
    ]

askRace m =
  div_ [class_ "animated fadeIn"] [
    displayRadioQuestion (Prelude.map Just races) m
                  characterRace "Your race?" RaceChecked 
    , nextButton RaceStage "Go to Life stance"
  ]

askAttitude m = 
  div_ [class_ "animated fadeIn"] [
    --displayRadioQuestion (Prelude.map Just lifeStances) m
    nextButton AttitudeStage "Go to Archetype"         
  ]

askFlawsAndTalents m = 
  div_ [class_ "animated fadeIn"] [
    h2_ [class_ "title is-2 has-text-weight-medium"] [ "Talents and flaws "]
    , maxTalentsInfo m $ TalentsMax 3 -- This is temporary
    , p_ [class_ "subtitle"] ["If you choose two flaws, you'll gain one additional talent."]
    , div_ [class_ "columns"] [
        div_ [class_ "column"] [
          displayCheckboxQuestion talents m
            characterTalent "What are your talents?" (TalentsMax 3) TalentChecked
        ]
        , div_ [class_ "column"] [
            displayCheckboxQuestion [ Alcoholic FlawLevel1
                        , Annoying FlawLevel1
                        , BadBack FlawLevel1
                        , BadSight FlawLevel1
                        , BadTempered FlawLevel1
                        , ChronicPain FlawLevel1,  Alcoholic FlawLevel1
                        , Annoying FlawLevel1
                        , BadBack FlawLevel1
                        , BadSight FlawLevel1
                        , BadTempered FlawLevel1
                        , ChronicPain FlawLevel1,  Alcoholic FlawLevel1
                        , Annoying FlawLevel1
                        , BadBack FlawLevel1
                        , BadSight FlawLevel1
                        , BadTempered FlawLevel1
                        , ChronicPain FlawLevel1, Alcoholic FlawLevel1
                        , Annoying FlawLevel1
                        , BadBack FlawLevel1
                        , BadSight FlawLevel1
                        , BadTempered FlawLevel1
                        , ChronicPain FlawLevel1] m characterFlaws "What are your flaws?" NoLimit FlawChecked
        ]
    ]
  ]

maxTalentsInfo :: Model -> MaxCheckbox -> View Msg
maxTalentsInfo m max = 
    div_ [ class_ ""]
    (
      case max of
        TalentsMax a -> 
          let 
            additionalTalents = 
              quot ( 
                S.size $ m ^. character . characterFlaws 
              ) 2
            toChoose = a + additionalTalents
            choosed = S.size $ m ^. character . characterTalent

          in 
            [ p_ [class_ "subtitle"] 
                $ ["You can choose "]
                ++ [ text $ ms $ toChoose - choosed]
                ++ [" more talents."]
            ]
        NoLimit -> []
    )

--dispaleyCheckboxQuestion :: [a] -> Model -> (characterField) -> String -> Int -> (a -> Bool -> View Msg)
displayCheckboxQuestion valueList model characterField question max msg =
  let 
    content = model ^. character . characterField

    additionalTalents = 
      case max of 
        TalentsMax _ -> 
          quot ( 
            S.size $ model ^. character . characterFlaws
            ) 2
        otherwise -> 0

    isDisabled x =  
      case max of 
        TalentsMax a -> Prelude.length content >= ( a + additionalTalents )
            && notElem x content 
        NoLimit -> False

    maxCheckbox = 
      case max of
        TalentsMax a -> Just (a + additionalTalents)
        NoLimit -> Nothing

  in
    div_ [ class_ "control has-text-centered" ] [
      div_ [] [
        p_ [class_ "title is-3 is-full has-text-weight-medium"] [ question ]
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
                      , onChecked $ msg x maxCheckbox
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
      h2_ [class_ "title is-2 has-text-weight-medium"] [ question ]
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

breadcrumb :: [Stage] -> Stage -> View Msg
breadcrumb stages active = 
  nav_ [class_ "breadcrumb is-centered"] [
    ul_ [] $ Prelude.map
        ( \x -> li_ [class_ 
                    (if (active == x) then "is-active" else "") ] [
          a_ [ onClick (ChangeStage x) ] [ text $ ms $ show x ]
        ])
        stages
    ]

menu = 
  div_ [class_ "columns is-mobile", style_ $ M.singleton "margin" "0rem 1rem 0rem 0.5rem"] [
      dropdownMenuItem "Hamingja" "You can spend your hamingja points on folowing benefits." 
                        "fa-theater-masks" hamingjaOptions
      , dropdownMenuItem "Gold" "" "fa-coins" []
      , dropdownMenuItem "Health" "" "fa-heartbeat" []
      --, dropdownMenuItem "Size" "" "fa-weight" []
  ]

dropdownMenuItem :: String -> String -> String -> [(Int, String)] -> View Msg  
dropdownMenuItem title description icon options = 
  let 
    formatOption ( points, option ) = 
        div_ [class_ "dropdown-item"] [
          div_ [class_ "columns is-mobile" ] [  
              span_ [class_ "column is-four-fifths"] [text option]
            , p_ [class_ "column", style_ $ M.singleton "margin" "0rem 0.5rem 0rem 0.5rem"] [
              text $ ms $ show points
            ] 
          ]
          , hr_ [class_ "dropdown-divider"]
        ]  
    dropUp = div_ [class_ ""]
  in
    div_ [ class_ "dropdown column"] [
        div_ [class_ "columns dropdown is-hoverable"] [
          div_ [class_ "column columns dropdown-trigger is-centered "] [
              button_ [class_ "button is-light is-rounded column is-large"
                    , style_ $ M.singleton "margin" "1rem"
                    , controls_ True
                      ] [
                        div_ [class_ "columns is-mobile"] [
                          span_ [class_ "title icon is-large column"] [
                            i_ [class_ $ ms $ "fas fa " ++ icon] []
                          ]
                          , span_ [class_ "column title"] [ text $ ms $ show 3 ]
                        ]     
                      ]  
          ]
          
          , div_ [class_ "dropdown-menu",  id_ "hamingja-menu"] [
              div_ [class_ "dropdown-content" ] 
              $ 
              [
                div_ [class_ "dropdown-item"] [
                    p_ [class_ "title"] [text $ ms title]
                    , p_ [class_ "subtitle"] [text $ ms description] 
                  ]
              ]
              ++ 
                if options /= [] 
                then 
                  [div_ [class_ "dropdown-item"] [
                    div_ [class_ "columns is-mobile" ] [
                      div_ [class_ "column is-four-fifths"] [
                          b_ [] [text "Option"]
                        ]
                        , div_ [class_ "column"] [
                            b_ [] [text "Cost"]
                        ]
                      ]
                    , hr_ [class_ "dropdown-divider"]
                    ]
                  ]
                    ++ (Prelude.map formatOption 
                        $ Prelude.map (\(x, y) -> (x, ms y)) options)
                else []
        ]
    ]
  ]
