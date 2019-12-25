module Update (updateModel) where

import       Control.Lens
import       Control.Monad.IO.Class
import       Data.Maybe
import qualified Data.Set as S
import       Miso
import       Miso.String
import       Model
import       Twarog
import qualified Twarog as T

-- | Updates model, optionally introduces side effects
updateModel :: Msg -> Model -> Effect Msg Model
updateModel (Name n) m = 
  noEff $ m & character . characterName .~ (Just $ fromMisoString n)

updateModel NoOp m = noEff m

updateModel (RaceChecked r (Checked True)) m = 
  noEff $ m & character . characterRace .~ r

updateModel (TalentChecked t max (Checked True)) m =  
  let 
    currTalents = m ^. character . characterTalent
    maxTalents = fromMaybe 0 max
  in
    noEff ( 
        if Prelude.length currTalents < maxTalents
        then m & character . characterTalent %~ S.insert t 
        else m 
        )
updateModel (TalentChecked r _ (Checked False)) m = 
  noEff $ m & character . characterTalent %~ S.delete r

updateModel (ChangeStage s) m = 
  let availableS = m ^. availableStages
      isNewStage = not $ elem s availableS
      current = m & currentStage .~ s
  in if isNewStage 
     then noEff $ current & availableStages .~ ( availableS ++ [s] )
     else noEff current

updateModel (SetCurrentRoll1 n) m = 
  let
    toString = fromMisoString n 
    result = case toString of
          "" -> 0
          _ -> read toString
  in
  noEff ( m & currentRoll1 .~ result)
                                                    
updateModel (SetCurrentRoll2 n) m = 
  let toString = fromMisoString n 
      result = case toString of
        "" -> 0
        _  -> read toString
  in noEff $ m & currentRoll2 .~  result

updateModel (SetAttribute n t) m =  
  let 
    action = 
      case t of
        Just Charisma -> cha
        Just Constitution -> con
        Just Dexterity -> dex
        Just Inteligence -> T.int
        Just Strength -> str
        Just WillPower -> wil
        Nothing -> cha

    next = 
      case t of 
        Just Every -> Just Charisma
        Just Charisma -> Just Constitution
        Just Constitution -> Just Dexterity
        Just Dexterity -> Just Inteligence
        Just Inteligence -> Just Strength
        Just Strength -> Just WillPower
        Just WillPower -> Nothing
        Nothing -> Nothing

    value = if n >= 18 then 18 else n
    attr = fromMaybe (Attributes 0 0 0 0 0 0) 
        (m ^. character . characterAttr)
    newModel = (((m & currentAttribBounce .~ t)
          & (character . characterAttr .~ Just (attr & action .~ value))
          & currentRoll1 .~ 0 )
          & currentRoll2 .~ 0 )
  in
    noEff $ newModel & currentStage .~ (AttribStage next)

updateModel (SexChecked s (Checked True)) m = 
  noEff $ m & character . characterSex .~ s
     
updateModel (FlawChecked f _ (Checked True)) m =  
  noEff 
    $ m & character . characterFlaws %~ S.insert f

updateModel (FlawChecked f _ (Checked False)) m = 
  noEff $ m & character . characterFlaws %~ S.delete f

updateModel (SetAllAttributes attr) m = 
  noEff  ((m & character . characterAttr .~ Just attr) 
        & currentAttribBounce .~ Just Every)
  
updateModel SetRandomAttr m = do
  m <# do
    attr <- sample $ genAttributes
    return $ SetAllAttributes attr

updateModel SetRandomBirth m = do
  m <# do
    birth <- sample $ genBirthday
    return $ SetBirth birth

updateModel (SetBirth b) m =
  noEff $ m & (character . characterBirth .~ Just b)
            . (character . characterTalent %~ if isMarked b
                                              then S.insert Marked
                                              else S.delete Marked)

updateModel (AddAvailableStage s) m =
  let 
    stages =  m ^. availableStages
  in
    noEff $ (m & availableStages .~ s : stages)
