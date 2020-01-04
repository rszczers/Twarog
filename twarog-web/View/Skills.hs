module View.Skills (
  askSkills
  , skillsSummary
  , skillsFirstScreen
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import       Data.Maybe
import qualified Data.Map         as M
import qualified Data.List        as L

import       View.Buttons

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
        (\x -> p_ [class_ "tag is-medium"] [text $ ms $ show x]) l
        
    animation = not (L.null roleSkills || L.null trainedSkills)
  in
    if animation then
      div_ [class_ "animated fadeIn", style_  $ M.singleton "margin-top" "1.5rem"] [
        div_ [class_ "columns"] [
          div_ [class_ "column is-one-third"] [
            p_ [class_ "title is-4"] ["Character role skills: "]
            , div_ [class_ "tags"] [ 
                showList roleSkills
            ]
          ]
          , div_ [class_ "column is-one-third"] [
            p_ [class_ "title is-4"] ["Trained role skills: "]
            , div_ [class_ "tags"] [ 
                showList trainedSkills
            ]
          ]
          , div_ [class_ "column"] [
            p_ [class_ "title is-4"] ["Untrained role skills: "]
            , div_ [class_ "tags"] [ 
                showList untrainedSkills
            ]
          ]
        ]
      ]
    else div_ [] [] 
    
skillsFirstScreen m = 
  div_ [class_ "animated fadeIn"] [
    p_ [class_ "title is-2 has-text-weight-medium"] [text "Your skills"] 
    , div_ [class_ "columns "] [
      div_ [class_ "level is-mobile column is-three-fifths is-offset-one-fifth"] [
        button_ [class_ "level-item button is-medium is-black"
            , onClick $ ChangeStage $ SkillsStage
            ] ["Choose skills by yourself"]   
        , label_ [class_ "level-item label is-medium"] [" or "]
        , button_ [class_ "level-item button is-medium is-black"
                , onClick SetRandomSkills
            ] ["Generate your skills"]
      ]  
    ]
    , div_ [] [
      skillsSummary m
    ]
    , nextButton RandomSkillsStage False
  ] 

askSkills m =
  let 
    role = m ^. character . characterRole 
    sex = fromMaybe Non $ m ^. character . characterSex
  in
    div_ [] [
      chooseSkills m CharacterRoleSkill 
          $ case role of
            Nothing -> 0
            Just a -> defaultCrSkillChoices a
      , chooseSkills m Trained $ if sex == Male then 1 else 2
      , chooseSkills m Untrained 0
      , button_ [class_ "button is-medium", onClick $ ChangeStage RandomSkillsStage  ] ["Ready!"]
    ] 


chooseSkills :: Model -> Proficiency -> Int -> View Msg
chooseSkills m p n =
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

    isActive = 
      n <= (case p of
            CharacterRoleSkill -> L.length roleSkills
            Trained -> L.length trainedSkills
            Untrained -> 0
            )

    question = case p of 
                  CharacterRoleSkill -> "Choose your role skills."
                  Trained -> "Your additional trained skills?"
                  Untrained -> "Your untrained skills"

    comment = case p of 
                CharacterRoleSkill -> ("Unchoosen skills will become your trained skills." 
                                      ++ "\n You can choose ") 
                                      ++ (show n 
                                      ++ " role skills")
                Trained -> ("You can choose " 
                            ++ show n )
                            ++ " addidional trained skills"
                Untrained -> ""

    skillList = case p of
            CharacterRoleSkill -> crSkills $ fromMaybe Civilian role
            Trained             -> (additionalTrainedSkills sex) L.\\ roleSkills 
            Untrained           -> skills L.\\ (crSkills $ fromMaybe Civilian role)

  in
    div_ [class_ "is-centered", style_  $ M.singleton "margin" "0.5rem"] [
      p_ [class_ "title is-3 is-full has-text-weight-medium"] [ question]
      , p_ [class_ "subtitle is-5"] [text $ ms comment]
      , div_ [ class_ "columns has-text-centered is-multiline is-mobile"]
        $ Prelude.map 
          (( \prof x ->
            label_ [class_ "label has-text-weight-normal"] [
              if prof == Untrained then
                div_ [class_ "tags" ] [ 
                  span_ [class_ "tag is-light is-medium "
                        , style_  $ M.singleton "margin" "0.5rem"] [ 
                    text $ ms $ show x
                  ]
                ]
              else
              div_ [class_ "field has-addons column has-text-centered is-one-fifth-tablet is-one-quarters-mobile"] [ 
                p_ [class_ "control"] [
                  input_ [ 
                    type_ "checkbox", name_ "skill"
                    , style_  $ M.singleton "margin" "0.5rem"
                    , checked_ $ elem x choosedSkills
                    , disabled_ $ isActive && (not $ elem x choosedSkills)
                    , onChecked $ SkillChecked x p
                  ]
                  ]
                , p_ [class_ "control"] [ 
                  text $ ms $ show x
                  ]
                ]
            ]
          ) p) skillList
    ]

