module View.Attitude (
  askAttitude
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import       Data.Maybe

import       View.Buttons

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
