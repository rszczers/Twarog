module View.Character (
  characterSummary
  , genCharacterScreen
) where

import      Miso
import      Miso.String
import      Model
import      Data.Text
import      Twarog

import      Control.Lens
import      Data.Maybe
import      View.FlawsAndTalents
import      View.Skills
import      Data.Map as M

characterSummary :: Model -> View Msg
characterSummary m = 
  let 
    char = m ^. character
    characterFiledText m field = 
        case m ^. character . field of
          Just ch -> show $ ch 
          Nothing -> ""
    sheet = fromMaybe emptySheet $ mkCharacterSheet $ m ^. character
      
    name = fromMaybe "" $ m ^. character . characterName
    attr = m ^. character . characterAttr
    race = characterFiledText m characterRace
    birth = characterFiledText m characterBirth
    sex = characterFiledText m characterSex
    lifeStance = characterFiledText m characterLifeStance
    attitude = characterFiledText m characterAlignment
    role = characterFiledText m characterRole
    flaws = m ^. character . characterFlaws
    talents = m ^. character . characterTalent
    notes = m ^. character . characterOther
  
    combatStats = sheet ^. sheetCombatStats
    toughness = sheet ^. sheetToughness
    resistance = sheet ^. sheetResistance

    toTable :: Attributes -> [(String, Int, String, String)]
    toTable a@(Attributes char con dex int str will) =
      let
        mods = modifiers a
        showMod mod = 
          if modifier > 0 then " +" ++ show modifier else show modifier
          where modifier = (mods ^. mod) 0
      in
        [("Charisma", char, "Cha", showMod chaMod)
        , ("Constitution", con, "Con", showMod conMod)
        , ("Dexterity", dex, "Dex", showMod dexMod)
        , ("Inteligence", int, "Int", showMod intMod)
        , ("Strength", str, "Str", showMod strMod)
        , ("Will Power", will, "Will", showMod wilMod) ]
      
  in 
    div_ [] [
      p_ [class_ "title is-2 has-text-weight-light"] ["Summary"] 
      , div_ [class_ "tile is-ancestor"] [ 
          div_ [class_ "tile is-parent"] [
            div_ [class_ "tile is-child box"] [
              p_ [class_ "heading"] ["Name"] 
              , p_ [class_ "title is-4"] [text $ ms $ name]
            ]
          ]
      ]
      , div_ [class_ "tile is-ancestor"] [ 
           div_ [class_ "tile is-parent"] [
              div_ [class_ "tile is-child box"] [
                p_ [class_ "heading"] ["Race"] 
                , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ race]
              ]
           ]
              , div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                    p_ [class_ "heading"] ["Character Role"] 
                    , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ role]
                  ] 
                ]
              , div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                      p_ [class_ "heading"] ["Life stance"] 
                      , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ lifeStance]
                    ]  
                  ]
                , div_ [class_ "tile is-parent"] [
                    div_ [class_ "tile is-child box"] [
                      p_ [class_ "heading"] ["Alignment"] 
                      , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ attitude]
                    ]
                  ]
              ]  
            , div_ [class_ "tile is-ancestor"] [
                div_ [class_ "tile is-parent"] [
                div_ [class_ "tile is-child box" ] [
                  p_ [class_ "heading"] ["Height"] 
                  , p_ [class_ "title is-4 has-text-weight-normal"] 
                        [text $ ms $ show $ sheet ^. sheetHeight ]
                ]
              ]
              , div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                    p_ [class_ "heading"] ["Size"] 
                    , p_ [class_ "title is-4 has-text-weight-normal"] 
                          [text $ ms $ show $ sheet ^. sheetSize]
                  ] 
                ]
              , div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                  p_ [class_ "heading"] ["Sex"] 
                  , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms sex]
                ] 
              ]
              , div_ [class_ "tile is-parent"] [
                div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                    p_ [class_ "heading"] ["Age"] 
                    , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ show $ sheet ^. sheetAge]
                ]
              ] 
              , div_ [class_ "tile is-parent"] [ 
                div_ [class_ "tile is-child box"] [
                  p_ [class_ "heading"] ["Max Age"] 
                  , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ show $ sheet ^. sheetMaxAge]
                ] 
              ]   
            ]
          ]
      , div_ [class_ "tile is-ancestor"] [ 
          div_ [class_ "tile is-parent"] [
            div_ [class_ "tile is-child box"] [
                p_ [class_ "heading"] ["Hamingja"] 
                , p_ [class_ "title is-4 has-text-weight-normal"] 
                      [text $ ms $ show $ sheet ^. sheetHamingja ]
              ] 
            ] 
            , div_ [class_ "tile is-parent"] [
                div_ [class_ "tile is-child box"] [
                  p_ [class_ "heading"] ["Expirience"] 
                  , p_ [class_ "title is-4 has-text-weight-normal"] 
                        [text $ ms $ show $ sheet ^. sheetExperience]
                ] 
              ] 
              , div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                    p_ [class_ "heading"] ["Level"] 
                    , p_ [class_ "title is-4 has-text-weight-normal"] 
                          [text $ ms $ show $ sheet ^. sheetLevel]
                ]   
              ]
          ]
      
      , div_ [class_ "tile is-ancestor"] [ 
          div_ [class_ "tile is-parent is-4 is-vertical"] [
            -- Show attributes and mods
            div_ [class_ "tile is-child"] [
              p_ [class_ "title is-4"] ["Attributes: "]
              , div_ [class_ "columns is-centered"] [
                  table_ [class_ "table is-narrow column"] $
                [
                  thead_ [] [
                    tr_ [] [
                      th_ [] ["Attribute"]
                      , th_ [] [""]
                      , th_ [] ["Modificator"]
                      , th_ [] [""]
                    ]
                  ]
                ]
                ++
                  (Prelude.map 
                    (\(w, x, y, z) -> 
                                    tr_ [] [ 
                                      td_ [] [text $ ms $ w]
                                      , td_ [] [text $ ms $ show x]
                                      , td_ [] [text $ ms $ y]
                                      , td_ [class_ "has-text-right"] [text $ ms z]
                                      ]
                    )
                    $ toTable $ fromMaybe (Attributes 0 0 0 0 0 0) attr  
                  )    
              ]
            ]
            , div_ [class_ "tile is-parent"][
                div_ [class_ "tile is-parent"] [
                  div_ [class_ "tile is-child box"] [
                        -- Show combat stats
                        p_ [class_ "title is-5"] ["Combat Statistics"]
                        , showCombatStats combatStats
                    ]
                  ]
                , div_ [class_  "tile is-parent is-vertical"] [
                    div_ [class_ "tile is-child box"] [
                      -- Show toughness
                      p_ [class_ "title is-5"] ["Toughness"]
                      , showThoughness toughness
                    ]
                    , div_ [class_ "tile is-child box"] [
                      -- Show resistance
                      p_ [class_ "title is-5"] ["Resistance"]
                      , showResistance resistance
                    ]
                  ]
                ]
            ]
            , div_ [class_ "tile is-parent is-vertical"] [
                  div_ [class_ "tile is-child"] [
                -- Show Skills
                    skillsSummary m 
                  ]
                  , div_ [class_ "tile is-child"] [
              -- Show Flaws and Talents
                  flawsAndTallentsSummary flaws talents
                  ]
              ]
          ]
          , div_ [class_ "tile is-ancestor"] [
              div_ [class_ "tile is-parent" ] [ 
                div_ [class_ "tile is-child"] [
              -- Show Notes
                  p_ [class_ "title is-5"] ["Notes"]
                  , div_ [] $                    
                    showNotes notes 
                  ]
              ]
            ]
             
          ]


genCharacterScreen m =
  div_ [class_ "animated fadeIn"] [
    p_ [class_ "title is-2 has-text-weight-medium"] [text "Your Character"] 
    , div_ [class_ "columns "] [
      div_ [class_ "level is-mobile column is-three-fifths is-offset-one-fifth"] [
        button_ [class_ "level-item button is-medium is-black"
            , onClick $ ChangeStage $ NameStage
            ] ["Construct your character step by step"]   
        , label_ [class_ "level-item label is-medium"] [" or "]
        , button_ [class_ "level-item button is-medium is-black"
                , onClick SetRandomCharacter
            ] ["Generate your character with one click"]
      ]  
    ]
  ] 

showStats list =
  let 
    showP (desc, value) = p_ [] [text $ ms $ desc ++ value]
  in 
    div_ [class_ ""] $ Prelude.map showP list

showCombatStats combatStats=
  let 
    cs :: CombatStats -> [(String, String)]
    cs CombatStats{..} =
      [ ("OV (ME): "        , show _ovMe)
      , ("DV (MI):"         , show _ovMi)
      , ("OV (ME): "        , show _dvMe)
      , ("DV (MI): "        , show _dvMi)
      , ("Dodging: "        , show _dodging)
      , ("Total AV: "       , show _totalAv)
      , ("MS penality: "    , show _msPenality)
      , ("Shield DV (ME): " , show _shieldDvMe) 
      , ("Shield Block: "   , show _shieldBlock) ]
  in
    showStats $ cs combatStats

showThoughness toughness = 
  let 
    st :: Toughness -> [(String, String)]
    st Toughness {..} = 
      [
          ("Cold: "         , show _toughnessCold ) 
        , ("Electricity: "  , show _toughnessElectricity )
        , ("Heat: "         , show _toughnessHeat )
        , ("Physical: "     , show _toughnessPhysical )
      ]
  in
    showStats $ st toughness

showResistance resistance =
  let
    res :: Resistance -> [(String, String)]
    res Resistance {..} =
      [
          ("Disease: "  , show _disease)
        , ("Poison: "   , show _poison)
      ]
  in
    showStats $ res resistance

showNotes notes =
  let 
    note :: Text -> View Msg
    note n = p_ [] [ text $ ms $ n ]
  in
    Prelude.map note notes