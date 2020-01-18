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
    , maxTalentsInfo m $ TalentsMax 3 -- TODO: Set max talents to choose
    , p_ [class_ "subtitle"] ["If you choose two flaws, you'll gain one additional talent."]
    , chooseRandomlyButton SetRandomFlawsAndTalents
    , div_ [class_ "columns"] [
        div_ [class_ "column"] [
            askTalents m
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
    tmpSheet = fromMaybe emptySheet $ mkCharacterSheet $ m ^. character

    fromTree :: (FlawLevel -> Flaw, [FlawLevel]) -> View Msg
    fromTree (flawConstructor, levels) = 
      div_ [class_ $ ms $ "message " 
                ++ if (Prelude.any (\x -> S.member x content) ( Prelude.map flawConstructor [FlawLevel1, FlawLevel2, FlawLevel3])) 
                  then "is-danger" else ""] [
        div_ [class_ "panel message-body", style_ $ M.singleton "margin" "0.5rem"] [
          div_ [class_ "panel-block"] [
            p_ [class_ "control label" ] [
              text $ ms $ show $ 
                flawConstructor FlawLevel1 
              ]
          ]
          , div_ [class_ "panel-block"] [
            div_ [class_ ""] [
              text $ ms  
                $ Prelude.head 
                $ (addMod tmpSheet 
                  $ 
                    case [ flawConstructor x | x <- levels, S.member (flawConstructor x) content ] of
                      [a] -> a
                      otherwise -> flawConstructor FlawLevel1
                  ) 
                    ^. sheetOther
            ]
          ]
        , div_ [class_ "panel-block"]
            $ Prelude.map 
            (\level -> button_ [class_ $ ms $ "button card-footer-item" 
                                ++ if S.member (flawConstructor level) content then " is-danger" else "" 
                                , onClick $ FlawChecked flawConstructor level Nothing $ Checked $ not (S.member (flawConstructor level) content)
                                ] 
              [text $ ms $ show level]) 
            levels
        ]
      ]
  in
    div_ [class_ ""] $
      [ 
        p_ [class_ "title is-3 is-full has-text-weight-medium"] [ "Your flaws?" ]
      ] 
      ++ (Prelude.map fromTree flawTree)
  
askTalents :: Model -> View Msg
askTalents m = 
  let 
    content = m ^. character . characterTalent
    tmpSheet = fromMaybe emptySheet $ mkCharacterSheet $ m ^. character
    checked x = S.member x content
    printNotes tal = 
      case (addMod tmpSheet $ tal) ^. sheetOther of
        [] -> text "" 
        a -> 
          div_ [class_ "has-text-right"] [
          ul_ [class_ "level-right"] $
            Prelude.map (\x -> li_ [] [text $ ms $ x]) a
          ]

    printTalent x = 
      div_ [  class_ $ ms $ "message " ++ if elem x content then "is-success" else ""
            , style_ $ M.singleton "margin" "0.5rem"] [
        div_ [class_ "message-body level", style_ $ M.singleton "padding" "0.5rem"] $ [
          label_ [class_ "label level-left"] [
            div_ [class_ "field has-addons has-text-centered"] [ 
              input_ [ 
                type_ "checkbox", name_ "talent"
                , style_  $ M.singleton "margin" "0.5rem"
                , checked_ $ elem x content
                , disabled_ $ False 
                , onChecked $ TalentChecked x (Just 4) -- TODO: max talents to choose
                ]
              , text $ ms $ show x
              ]
          ]
          , printNotes x
        ]
     
      ]
  in
    div_ [class_ ""] $
      [ p_ [class_ "title is-3 is-full has-text-weight-medium"] 
          [ "Your talents?" ]
      ] 
        ++ (Prelude.map printTalent talents)
  

maxTalentsInfo :: Model -> MaxCheckbox -> View Msg -- TODO: decide how many talents one can choose,
maxTalentsInfo m max =                             --       depends on choosed flaws.
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
