module View.FlawsAndTalents(
  askFlawsAndTalents
  , flawsAndTalentsFirstScreen
  , flawsAndTallentsSummary
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import       Data.Maybe
import qualified Data.Map         as M
import qualified Data.Set         as S

import       View.Buttons

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
          ]
    ] 
    , nextButton (FlawsAndTalentsStage True) $ ((talents) == []) 
                                              || ( S.null $ m ^. character . characterTalent)
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
              flawsAndTallentsSummary flaws talents
            ]
          else [] 
          ) 
        , nextButton (FlawsAndTalentsStage True) $ (S.null talents) 
    ] 

flawsAndTallentsSummary :: S.Set Flaw -> S.Set Talent -> View Msg
flawsAndTallentsSummary flaws talents =
  div_ [class_ "columns " ] [
    div_ [class_ "column animated bounceInLeft"] [
      h4_ [class_ "title"] ["Talents"]
      , div_ [class_ "tags"] $ 
          Prelude.map  
            (\x -> span_ [class_ "tag is-success is-light is-medium"] [text $ ms $ show x]) 
            (S.toList $ talents)
      ]
      , div_ [class_ "column animated bounceInRight"] [
          h4_ [class_ "title"] ["Flaws"]
          , div_ [class_ "tags"] $ 
              Prelude.map  
                (\x -> span_ [class_ "tag is-danger is-light is-medium"] [text $ ms $ show x]) 
                (S.toList $ flaws)
      ]
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
