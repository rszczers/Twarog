module ViewElements.Menu (
  menu
  , navbarElem
  , breadcrumb
) where

import       Miso
import       Miso.String
import       Model
import       Twarog

import       Control.Lens
import qualified Data.Map         as M
import qualified Data.List        as L

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
  let 
    cleanStages :: [Stage] -> [Stage]
    cleanStages s = 
      ((s L.\\ [AttribStage (Just a) | a <- attrBounce])
      L.\\ [FlawsAndTalentsStage True])
      L.\\ [SkillsStage]
  in
    nav_ [class_ "breadcrumb is-centered"] [
      ul_ [] $ Prelude.map
          ( \x -> li_ [class_ 
                      (if (active == x) then "is-active" else "") ] [
            a_ [ onClick (ChangeStage x) ] [ text $ ms $ show x ]
          ])
          $ cleanStages stages
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