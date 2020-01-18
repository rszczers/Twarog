module View.Buttons 
(
  nextButton
  , chooseRandomlyButton
  , displayRadioQuestion
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import qualified Data.Set         as S
import qualified Data.Map         as M

nextButton :: Stage -> Bool -> View Msg                            
nextButton stage isActive = 
  div_ [class_ "columns"][
    div_ [class_ "column is-full level"] [
      div_ [ class_ "level-right"] [
        button_ [ 
          class_ $ ms $ "button is-outlined is-medium " 
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

--dispaleyRadioQuestion :: [a] -> Model -> (characterField) -> String ->  (a -> Bool -> View Msg)
displayRadioQuestion valueList model characterField question msg =
  let 
    content = model ^. character . characterField 
  in
    div_ [ class_ "control has-text-centered" ] [
      h2_ [class_ "title is-2 has-text-weight-medium"] [ question ]
      , div_ [ class_ "columns is-centered is-multiline is-mobile" ]
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
