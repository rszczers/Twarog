module ViewElements.Birthday
(
  askBirthday
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import       Data.Maybe
import qualified Data.Map         as M

import      ViewElements.Buttons

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
