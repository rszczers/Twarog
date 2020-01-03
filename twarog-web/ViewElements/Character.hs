module ViewElements.Character (
  characterSummary
  , genCharacterScreen
) where

import      Miso
import      Miso.String
import      Model
import      Twarog

import      Control.Lens
import      Data.Maybe
import      ViewElements.FlawsAndTalents
import      ViewElements.Skills

characterSummary :: Model -> View Msg
characterSummary m = 
  let 
    char = m ^. character
    characterFiledText m field = 
        case m ^. character . field of
          Just ch -> show $ ch 
          Nothing -> ""
      
    name = characterFiledText m characterName
    attr = m ^. character . characterAttr
    race = characterFiledText m characterRace
    birth = characterFiledText m characterBirth
    sex = characterFiledText m characterSex
    lifeStance = characterFiledText m characterLifeStance
    attitude = characterFiledText m characterAlignment
    role = characterFiledText m characterRole
    flaws = m ^. character . characterFlaws
    talents = m ^. character . characterTalent

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
      p_ [class_ "title"] ["Summary"]
      , div_ [class_ "is-centered"] [
          p_ [class_ "heading"] ["Name"] 
          , p_ [class_ "title is-4"] [text $ ms $ name]
      ]
      , div_ [class_ "columns"] [
          div_ [class_ "column"] [
             p_ [class_ "heading"] ["Race"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ race]
          ]
          , div_ [class_ "column"] [
            p_ [class_ "heading"] ["Birth"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ birth]
          ] 
          , div_ [class_ "column"] [
            p_ [class_ "heading"] ["Sex"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ sex]
          ]  
      ]
      , div_ [class_ "columns"] [
          div_ [class_ "column"] [
            p_ [class_ "heading"] ["Life Stance"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ lifeStance]
          ]
          , div_ [class_ "column"] [
            p_ [class_ "heading"] ["Attitude"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ attitude]
          ] 
          , div_ [class_ "column"] [
            p_ [class_ "heading"] ["Role"] 
            , p_ [class_ "title is-4 has-text-weight-normal"] [text $ ms $ role]
          ]  
      ]
      -- Show attributes and mods
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
      -- Show Flaws and Talents
      , flawsAndTallentsSummary flaws talents
      -- Show Skills
      , skillsSummary m  
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