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
import qualified Data.List        as L

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
        SkillsStage Untrained       -> skillsSummary m
        SkillsStage prof            -> askSkills m prof

nextButton :: Stage -> Bool -> View Msg                            
nextButton stage isActive = 
  div_ [class_ "columns"][
    div_ [class_ "column is-full level"] [
      div_ [ class_ "level-right"] [
        button_ [ class_ $ ms $ "button is-outlined is-medium " 
                            ++ if not isActive then "animated tada" else "" 
                , onClick $ ChangeStage $ nextStage stage
                , disabled_  isActive ] 
                [ 
                  text $ getNextButtonText $ nextStage stage
                  , span_ [class_ "icon", style_ $ M.singleton "padding-left" "1.5rem"] [
                      i_ [class_ "fas fa-chevron-right"] []
                    ]
                 ]
        ]
      ]
    ]  

chooseRandomlyButton :: Msg -> View Msg
chooseRandomlyButton whatToDo = 
  button_ [class_ "button is-medium", onClick whatToDo, style_ $ M.singleton "margin" "1rem"] [
    div_ [class_ "columns is-mobile"] [
      span_ [class_ "column title icon is-large column"] [
        i_ [class_ $ "fas fa-dice"] []
      ]
      , span_ [class_ "column"] ["Choose randomly"]
    ]
  ]

askName :: Model -> View Msg
askName m =
  let 
    name = m^.character.characterName
  in
    div_ [class_ "animated fadeIn"]
      [ h2_ [class_ "title is-2 has-text-weight-medium"] [ "Your name? "]
      , div_ [class_ "columns is-centered"] [
          div_ [ class_ "column is-two-fifths"] [
              input_ [ class_ "input is-medium"
                , value_ $ ms
                  $ fromMaybe "" name
                , onInput Name 
                ]
            ]
          ]
          , nextButton NameStage (name ==  Nothing || name == Just "")
      ]

askAttributes :: Model -> Maybe AttribBounce -> View Msg
askAttributes m bounce = 
  let 
    firstScreen = isNothing bounce
    attr = printBounce bounce
    max3D6 = 18
    isBtnActive = case m ^. character . characterAttr of  
                    Nothing -> True
                    Just (Attributes 0 0 0 0 0 0) -> True
                    otherwise -> False
    isNotValid = not $ (Prelude.foldr max max3D6 [m ^. currentRoll1, m ^. currentRoll2]) == max3D6
  in  
    if firstScreen 
    then attributesFirstScreen isBtnActive
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

attributesFirstScreen :: Bool -> View Msg
attributesFirstScreen isBtnActive = div_ [class_ "animated fadeIn"] [
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
              , nextButton (AttribStage Nothing) isBtnActive
            ] 

askBirthday ::  Model -> View Msg
askBirthday m =
  let
    birthday = m ^. character . characterBirth
    isBtnActive = isNothing birthday
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
        , nextButton BirthStage isBtnActive
    ]

askSex :: Model -> View Msg
askSex m = 
  div_ [class_ "animated fadeIn"] [    
    displayRadioQuestion (Prelude.map Just sexes) m 
                        characterSex "Your sex?" SexChecked
    , chooseRandomlyButton SetRandomSex
    , nextButton SexStage $ isNothing $ m ^. character . characterSex
    ]

askRace :: Model -> View Msg
askRace m =
  div_ [class_ "animated fadeIn"] [
    displayRadioQuestion (Prelude.map Just playableRaces) m
                  characterRace "Your race?" RaceChecked 
    , chooseRandomlyButton SetRandomRace
    , nextButton RaceStage $ isNothing $ m ^. character . characterRace
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

askAttitude :: Model -> View Msg
askAttitude m = 
  div_ [] [
    p_ [class_ "title is-2 has-text-weight-medium"] ["Your attitude?"]
    , chooseAttitide m
    , showArchetype m
    , chooseRandomlyButton SetRandomArchetype
    , nextButton AttitudeStage $ isNothing $ m ^. character . characterAlignment
  ]

showArchetype :: Model -> View Msg
showArchetype m =
  let 
    arch  = m ^. character . characterAlignment
  in
    p_ [class_ "subtitle"] [
      ( case arch of 
          Just a ->  (text $ ms $ "Your archetype is "  ++ show a)
          Nothing -> (text $ "Your character is neutral")
        )
    ]

chooseAttitide :: Model -> View Msg
chooseAttitide m =
  let
    social = m ^. sociability
    submiss = m ^. submissiveness
    ont = m ^. ontology
    emp = m ^. empathy
  in
    div_ [class_ "columns is-centered"] [
      div_ [class_ "column"] []
      , div_ [class_ "column"] [
        div_ [class_ "buttons has-addons is-centered"] [
          button_ 
          [
            class_ 
              (ms $ "button"  
              ++ if social == (Just Introvert) then " is-link" else "")
              , onClick $ SetSociability $ Just Introvert
          ] 
          [
            span_ [class_ "icon"] [
              i_ [class_ "fas fa-user"] []
            ]
            , span_ [] ["Introvert" ]
          ]
          , button_ 
          [
            class_ 
              (ms $ "button"  
              ++ if social == (Nothing) then " is-link" else "")
              , onClick $ SetSociability $ Nothing
          ] 
          [
            span_ [] ["Neutral" ]
          ]
          , button_ 
            [
              class_ 
                (ms $ "button"  
                ++ if social == (Just Extravert) then " is-link" else "")
                , onClick $ SetSociability $ Just Extravert
            ] 
            [
              span_ [] ["Extravert"]
              , span_ [class_ "icon"] [
                i_ [class_ "fas fa-users"] []
                ]
            ]
        ]
        , div_ [class_ "buttons has-addons is-centered"] [
          button_ 
          [
            class_ 
              (ms $ "button"  
              ++ if submiss == (Just Lawful) then " is-link" else "")
              , onClick $ SetSubmissiveness $ Just Lawful
          ] 
          [
            span_ [class_ "icon"] [
              i_ [class_ "fas fa-gavel"] []
            ]
            , span_ [] ["Lawful" ]
          ]
          , button_ 
          [
            class_ 
              (ms $ "button"  
              ++ if submiss == (Nothing) then " is-link" else "")
              , onClick $ SetSubmissiveness $ Nothing
          ] 
          [
            span_ [] ["Neutral" ]
          ]
          , button_ 
            [
              class_ 
                (ms $ "button"  
                ++ if submiss == (Just Anarchic) then " is-link" else "")
                , onClick $ SetSubmissiveness $ Just Anarchic
            ] 
            [
              span_ [] ["Anarchic"]
              , span_ [class_ "icon"] [
                i_ [class_ "fab fa-freebsd"] []
                ]
            ]
          ]
          , div_ [class_ "buttons has-addons is-centered"] [
            button_ 
            [
              class_ 
                (ms $ "button"  
                ++ if ont == (Just Spiritual) then " is-link" else "")
                , onClick $ SetOnthology $ Just Spiritual
            ] 
            [
              span_ [class_ "icon"] [
                i_ [class_ "fas fa-ghost"] []
              ]
              , span_ [] ["Spiritual" ]
            ]
            , button_ 
            [
              class_ 
                (ms $ "button"  
                ++ if ont == (Nothing) then " is-link" else "")
                , onClick $ SetOnthology $ Nothing
            ] 
            [
              span_ [] ["Neutral" ]
            ]
            , button_ 
              [
                class_ 
                  (ms $ "button"  
                  ++ if ont == (Just Materialistic) then " is-link" else "")
                  , onClick $ SetOnthology $ Just Materialistic
              ] 
              [
                span_ [] ["Materialistic"]
                , span_ [class_ "icon"] [
                  i_ [class_ "fas fa-dollar-sign"] []
                  ]
              ]
            ]
            , div_ [class_ "buttons has-addons is-centered"] [
              button_ 
              [
                class_ 
                  (ms $ "button"  
                  ++ if emp == (Just Compasionate) then " is-link" else "")
                  , onClick $ SetEmpathy $ Just Compasionate
              ] 
              [
                span_ [class_ "icon"] [
                  i_ [class_ "fas fa-hand-holding-heart"] []
                ]
                , span_ [] ["Compasionate" ]
              ]
              , button_ 
              [
                class_ 
                  (ms $ "button"  
                  ++ if emp == (Nothing) then " is-link" else "")
                  , onClick $ SetEmpathy $ Nothing
              ] 
              [
                span_ [] ["Neutral" ]
              ]
              , button_ 
                [
                  class_ 
                    (ms $ "button"  
                    ++ if emp == (Just Cruel) then " is-link" else "")
                    , onClick $ SetEmpathy $ Just Cruel
                ] 
                [
                  span_ [] ["Cruel"]
                  , span_ [class_ "icon"] [
                    i_ [class_ "fas fa-hand-rock"] []
                    ]
                ]
              ]
      ]
      , div_ [class_ "column"] []
    ]

flawsAndTalentsFirstScreen :: Model -> View Msg
flawsAndTalentsFirstScreen m = 
  let 
    flaws = m ^. character . characterFlaws
    talents = m ^. character . characterTalent
    isVisible = not $ (S.null flaws) && (S.null talents) 
  in
    div_ [class_ "animated fadeIn"] [
      p_ [class_ "title is-2 has-text-weight-medium"] [text "Choose talents and flaws"] 
      , div_ [class_ "columns "] [
        div_ [class_ "level is-mobile column is-three-fifths is-offset-one-fifth"] [
          button_ [class_ "level-item button is-medium "
              , onClick $ ChangeStage $ FlawsAndTalentsStage True
              ] [
                div_ [class_ "columns is-mobile"] [
                  span_ [class_ "column title icon is-large column"] [
                    i_ [class_ $ "fas fa-list"] []
                  ]
                  , span_ [class_ "column"] ["Choose from list"]
                ]
              ]   
          , label_ [class_ "level-item label is-medium"] [" or "]
          , chooseRandomlyButton SetRandomFlawsAndTalents
          ] 
        ]
        , div_ [] 
          ( 
          if isVisible
          then
            [ 
              div_ [class_ "columns " ] [
                div_ [class_ "column animated bounceInLeft"] [
                  h4_ [class_ "title"] ["Talents"]
                  , div_ [class_ "tags"] $ 
                      Prelude.map  
                        (\x -> span_ [class_ "tag is-success is-light is-large"] [text $ ms $ show x]) 
                        (S.toList $ talents)
                ]
                , div_ [class_ "column animated bounceInRight"] [
                  h4_ [class_ "title"] ["Flaws"]
                  , div_ [class_ "tags"] $ 
                      Prelude.map  
                        (\x -> span_ [class_ "tag is-danger is-light is-large"] [text $ ms $ show x]) 
                        (S.toList $ flaws)
                ]
              ]
          ]
          else [] 
          ) 
        , nextButton (FlawsAndTalentsStage True) $ (S.null talents) 
    ] 

askFlawsAndTalents :: Model -> View Msg
askFlawsAndTalents m = 
  div_ [class_ "animated fadeIn"] [
    h2_ [class_ "title is-2 has-text-weight-medium"] [ "Talents and flaws "]
    , maxTalentsInfo m $ TalentsMax 3 -- This is temporary
    , p_ [class_ "subtitle"] ["If you choose two flaws, you'll gain one additional talent."]
    , chooseRandomlyButton SetRandomFlawsAndTalents
    , div_ [class_ "columns"] [
        div_ [class_ "column"] [
          displayCheckboxQuestion talents m
            characterTalent "What are your talents?" (TalentsMax 3) TalentChecked
        ]
        , div_ [class_ "column"] [
            askFlaws m
            --displayCheckboxQuestion flaws m characterFlaws "What are your flaws?" NoLimit FlawChecked
        ]
    ] 
    , nextButton (FlawsAndTalentsStage True) $ ((talents) == []) 
                                              || ( S.null $ m ^. character . characterTalent)
  ]

askFlaws :: Model -> View Msg
askFlaws m = 
  let 
    content = m ^. character . characterFlaws

    fromTree :: (FlawLevel -> Flaw, [FlawLevel]) -> View Msg
    fromTree (flawConstructor, levels) = 
      div_ [class_ "level"] [
        div_ [class_ "level-item"] [
          p_ [class_ "control"] [
            input_ [ 
              type_ "checkbox", name_ "talent"
              , style_  $ M.singleton "margin" "0.5rem"
              , checked_ $ Prelude.any (\x -> S.member x content) ( Prelude.map flawConstructor [FlawLevel1, FlawLevel2, FlawLevel3]) 
              , onChecked $ FlawChecked flawConstructor FlawLevel1 Nothing
            ]
            , text $ ms $ show $ flawConstructor FlawLevel1
          ]
        ]
        , div_ [class_ "level-item"] [
            div_ [class_ "buttons has-addons"] $
              Prelude.map 
              (\level -> button_ [class_ $ ms $ "button" 
                                  ++ if S.member (flawConstructor level) content then " is-link" else "" 
                                  , onClick $ FlawChecked flawConstructor level Nothing $ Checked $ not (S.member (flawConstructor level) content)
                                  ] 
                [text $ ms $ show level]) 
              levels
            
        ] 
      ]
  in
    div_ [] $
      [ p_ [class_ "title is-3 is-full has-text-weight-medium"] 
          [ "Your flaws?" ]
      ] 
        ++ (Prelude.map fromTree flawTree)
  


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

skillsSummary m = 
  let 
    roleSkills = M.keys $ M.filterWithKey (\_ k -> (k ^. proficiency) == CharacterRoleSkill) 
        $ m ^. character . characterSkills
    trainedSkills = M.keys $ M.filterWithKey (\_ k -> (k ^. proficiency) == Trained) 
        $ m ^. character . characterSkills
    untrainedSkills = M.keys $ M.filterWithKey (\_ k -> (k ^. proficiency) == Untrained) 
        $ m ^. character . characterSkills
    showList l = 
      div_ [] $
        Prelude.map 
        (\x -> div_ [] [text $ ms $ show x]) l
  in
    div_ [] [
      p_ [class_ "title is-4"] ["Character role skills: "]
      , showList roleSkills
      , p_ [class_ "title is-4"] ["Trained role skills: "]
      , showList trainedSkills
      , p_ [class_ "title is-4"] ["Untrained role skills: "]
      , showList untrainedSkills
    ]
    
askSkills :: Model -> Proficiency -> View Msg
askSkills m p =
  let
    role = m ^. character . characterRole
    sex = fromMaybe Non $ m ^. character . characterSex
    attr = fromMaybe (Attributes 0 0 0 0 0 0) $ m ^. character . characterAttr
    roleSkills = M.keys $ M.filterWithKey (\_ k -> (k ^. proficiency) == CharacterRoleSkill) 
        $ m ^. character . characterSkills
    trainedSkills = M.keys $ M.filterWithKey (\_ k -> (k ^. proficiency) == Trained) 
        $ m ^. character . characterSkills
    choosedSkills = 
      case p of
        CharacterRoleSkill  -> roleSkills
        Trained             -> trainedSkills
        Untrained           -> []

    question = case p of 
                  CharacterRoleSkill -> "Your role skills?"
                  Trained -> "Your trained skills?"
                  Untrained -> ""

    skillList = case p of
            CharacterRoleSkill -> crSkills $ fromMaybe Civilian role
            Trained             -> (additionalTrainedSkills sex) L.\\ roleSkills
            Untrained           -> []

  in
    div_ [] [
      p_ [class_ "title is-3 is-full has-text-weight-medium"] [ question]
      , div_ [ class_ "columns has-text-centered is-multiline is-mobile"]
        $ Prelude.map 
          ( \x ->
            label_ [class_ "label has-text-weight-normal"] [
              div_ [class_ "field has-addons column has-text-centered is-one-fifth-tablet is-one-quarters-mobile"] [ 
                p_ [class_ "control"] [
                  input_ [ 
                    type_ "checkbox", name_ "skill"
                    , style_  $ M.singleton "margin" "0.5rem"
                    , checked_ $ elem x choosedSkills
                    --, disabled_ $ isDisabled x
                    , onChecked $ SkillChecked x p
                  ]
                  ]
                , p_ [class_ "control"] [ 
                  text $ ms $ show x
                  ]
                ]
              ]
          ) skillList
          , nextButton (SkillsStage p ) $ choosedSkills == []
    ]


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

navbarElem :: Model -> View Msg
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

menu :: View Msg
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
