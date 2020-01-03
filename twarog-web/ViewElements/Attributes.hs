module ViewElements.Attributes 
( 
  askAttributes
, attributesFirstScreen 
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import qualified Data.Map         as M
import       Data.Maybe

import       ViewElements.Buttons

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
