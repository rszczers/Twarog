module Update (updateModel) where

  import           Control.Lens
  import           Control.Monad.IO.Class
  import           Data.Maybe
  import           Miso
  import           Miso.String
  import           Model
  import           Twarog.Backend.Character
  import           Twarog.Backend.Talents
  import           Twarog.Backend.Types
  
  -- | Updates model, optionally introduces side effects
  updateModel :: Msg -> Model -> Effect Msg Model
  updateModel (Name n) m = 
    noEff $ m & character . characterName .~ (Just $ fromMisoString n)
  updateModel NoOp m = noEff m
  updateModel (RaceChecked r (Checked True)) m = 
    noEff $ m & character . characterRace .~ r
  updateModel (TalentChecked t (Checked True)) m =  
    let 
      currTalents = fromMaybe [] $ m ^. character . characterTalent
    in
      noEff ( 
        if Prelude.length currTalents < maxTalents
        then m & character . characterTalent .~ Just (currTalents ++ [ t ])
        else m 
        )
  updateModel (TalentChecked r (Checked False)) m = 
    let 
      currTalents = fromMaybe [] $ m ^. character . characterTalent
    in
      noEff $ m & character . characterTalent .~
        (Just $ Prelude.filter (/= r) currTalents)
  updateModel (ChangeStage s) m = noEff $ m & currentStage .~ s
  updateModel (SetCurrentRoll1 n) m = 
    let
      toString = fromMisoString n 
      result = case toString of
                  "" -> 0
                  _ -> read toString
    in
      noEff ( m & currentRoll1 .~  result)
                                                                                                          
  updateModel (SetCurrentRoll2 n) m = 
    let
      toString = fromMisoString n 
      result = case toString of
                  "" -> 0
                  _ -> read toString
    in
      noEff ( m & currentRoll2 .~  result)

  updateModel (SetAttribute n t) m =  
    if t <= 6
    then
      let 
        action = case t of
          1 -> cha
          2 -> con
          3 -> dex
          4 -> int
          5 -> str
          6 -> wil
        newT = case t of
                6 -> 1
                _ -> t +1
        value = if n >= 18 then 18 else n
        atr = fromMaybe (Attributes 0 0 0 0 0 0) 
                  (m ^. character . characterAttr)
        newModel = ((m & (character . characterAttr .~ Just (atr & action .~ value))
                    & currentRoll1 .~ 0 )
                    & currentRoll2 .~ 0 )
        
      in
        noEff $ newModel & currentStage .~ (AtribStage newT)
    else
      noEff $ m & currentStage .~ (AtribStage 1)
                