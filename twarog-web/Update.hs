module Update (updateModel) where

import       Control.Lens as L
import       Control.Monad.IO.Class
import       Data.Maybe
import qualified Data.Set as S
import       Data.Map     as M
import       Miso
import       Miso.String
import       Model
import       Twarog
import qualified Twarog as T
import qualified Data.Text as Tx

-- | Updates model, optionally introduces side effects
updateModel :: Msg -> Model -> Effect Msg Model
updateModel (Name n) m = 
  noEff $ m & character . characterName .~ (Just $ fromMisoString n)

updateModel (SuggestRandomNames name) m = 
   m <# do
    names <- sample $ genNames (Tx.pack $ fromMisoString name) 
    return $ SetSugestedNames names name

updateModel (SetSugestedNames n name) m = 
  noEff $ (m & possibleNames .~ (Prelude.take 20 n)) & character . characterName .~ (Just $ fromMisoString name)

updateModel NoOp m = noEff m

updateModel (RaceChecked r (Checked True)) m = 
  noEff (m & character . characterRace .~ r)
  --(m & character . characterRace .~ r) <# do return $ SetRandomLifeStance

updateModel (LifeStanceChecked l (Checked True)) m = 
  noEff (m & character . characterLifeStance .~ l)

updateModel (ArchetypeChecked a (Checked True)) m = 
  noEff $ (m & character . characterAlignment .~ a)

updateModel (RoleChecked a (Checked True)) m = 
  noEff $ (m & character . characterRole .~ a)

updateModel (TalentChecked t max (Checked True)) m =  
  let 
    currTalents = m ^. character . characterTalent
    --maxTalents = fromMaybe 0 max
  in
    noEff ( 
        --if Prelude.length currTalents < maxTalents
        --then 
          m & character . characterTalent %~ S.insert t 
        --else m 
        )
updateModel (TalentChecked r _ (Checked False)) m = 
  noEff $ m & character . characterTalent %~ S.delete r

updateModel (ChangeStage s) m = 
  let 
    availableS = m ^. availableStages
    
    isNewStage a m = 
      if not $ elem a availableS
      then current & availableStages .~ ( availableS ++ [s] )
      else current
    current = m & currentStage .~ s
    skillStage a m = 
      if a == SkillsStage 
      then m & character.characterSkills .~ 
            (M.fromList . Prelude.zip skills $ repeat (CharacterSkill 0 Untrained))
      else m
  in
    noEff $ skillStage s $ isNewStage s m

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

updateModel (FlawChecked flawConstructor flawLvl _ (Checked True)) m =  
  let 
    flaws = m ^. character . characterFlaws
  in
    noEff 
      $ m & character . characterFlaws .~ 
        (S.insert (flawConstructor flawLvl) 
          $ newFlawSet flaws flawConstructor flawLvl )

updateModel (FlawChecked flawConstructor flawLvl _ (Checked False)) m =
  let 
    flaws = m ^. character . characterFlaws
  in 
    noEff $ m & character . characterFlaws .~ newFlawSet flaws flawConstructor flawLvl

updateModel (SkillChecked s p (Checked True)) m =
  let 
    chSkills = m ^. character . characterSkills
    model = if (M.null chSkills) 
      then  m & character . characterSkills 
            .~ (M.fromList $ Prelude.zip skills $ repeat (CharacterSkill 0 Untrained) )
      else m
  in 
    noEff $ model & character . characterSkills . L.at s . _Just . proficiency .~ p

updateModel (SkillChecked s p (Checked False)) m =
  let 
    chSkills = m ^. character . characterSkills
    model = if (M.null chSkills) 
      then  m & character . characterSkills 
            .~ (M.fromList $ Prelude.zip skills $ repeat (CharacterSkill 0 Untrained) )
      else m
  in 
    noEff $ model & character . characterSkills . L.at s . _Just . proficiency .~ Untrained


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

updateModel SetRandomRole m = do
    m <# do
      let 
        attr = fromMaybe (Attributes 0 0 0 0 0 0) $ m ^. character . characterAttr
        talents = S.toList $ m ^. character . characterTalent
        lifeStance = fromMaybe Traditional $ m ^. character . characterLifeStance
        race = fromMaybe Gnome $ m ^. character . characterRace
        sex = fromMaybe Non $ m ^. character . characterSex
        archetype = fromMaybe Kronic $ m ^. character . characterAlignment
      role <- sample $ genCharacterRole attr talents lifeStance race sex archetype
      return $ SetRole role

updateModel (SetRole r) m =
  noEff $ m & (character . characterRole .~ Just r)

updateModel SetRandomSkills m = do
    m <# do
      let 
        attr = fromMaybe (Attributes 0 0 0 0 0 0) $ m ^. character . characterAttr
        sex = fromMaybe Non $ m ^. character . characterSex
        role = fromMaybe Civilian $ m ^. character . characterRole
      skills <- sample $ genInitCharacterSkills role sex (modifiers attr)
      return $ SetSkills skills

updateModel (SetSkills s) m =
  noEff $ m & (character . characterSkills .~ s)

updateModel SetRandomRace m = do
  m <# do
    race <- sample $ genRace'
    return $ SetRace race

updateModel (SetRace b) m =
  noEff (m & character . characterRace .~ Just b)
  -- (m & character . characterRace .~ Just b) <# do return $ SetRandomLifeStance

updateModel SetRandomSex m = do
  m <# do
    sex <- sample $ 
          (case m ^. character . characterRace of 
            Just r -> genSex' r 
            Nothing -> genSex
          ) 
    return $ SetSex sex

updateModel (SetSex s) m =
  noEff $ m & character . characterSex .~ Just s

updateModel SetRandomFlawsAndTalents m = do
  m <# do
    flaws <- sample $ genFlaws
    talents <- sample 
                $ genTalents 
                    (case (m ^. character . characterRace) of
                      Just t -> t
                      Nothing -> Elf)
                    flaws $ 
                          if isMarked 
                            $ fromMaybe (Birthday (CommonDay 1) Valaskjolf) (m ^. character . characterBirth)
                          then S.singleton Marked
                          else S.empty
    return $ SetFlawsAndTalents talents flaws

updateModel (SetFlawsAndTalents t f) m =
  noEff $ (m & character . characterFlaws .~ f)
            & character . characterTalent .~ t
  
updateModel SetRandomLifeStance m = do
  m <# do
    lifeStance <- sample $ 
                    case (m ^. character .characterRace) of 
                      Just a -> genLifeStance' a
                      Nothing -> genLifeStance    
    return $ SetLifeStance lifeStance

updateModel (SetLifeStance l) m =
  noEff $ m & character . characterLifeStance .~ Just l
            
updateModel (AddAvailableStage s) m =
  let 
    stages =  m ^. availableStages
  in
    noEff $ (m & availableStages .~ s : stages)

updateModel (SetSociability s) m =
  noEff $ setModelArchetype (m & sociability .~ s)

updateModel (SetSubmissiveness s) m =
  noEff $ setModelArchetype $ m & submissiveness .~ s

updateModel (SetOnthology o) m =
  noEff $ setModelArchetype $ m & ontology .~ o

updateModel (SetEmpathy e) m =
  noEff $ setModelArchetype $ m & empathy .~ e

updateModel SetRandomArchetype m = do
  m <# do
    arch <- sample $ genArchetype
    return $ SetArchetype arch

updateModel (SetArchetype a) m =
  noEff $ setModelAttitude (attitude a) $ m & character . characterAlignment .~ Just a

updateModel SetRandomCharacter m = do
  (m & currentStage .~ SummaryStage) <# do
    char <- sample $ genNewCharacter
    return $ SetCharacter $ char

updateModel (SetCharacter ch) m =
  noEff $ m & character .~ ch

newFlawSet :: S.Set Flaw -> (FlawLevel -> Flaw) -> FlawLevel -> S.Set Flaw
newFlawSet fList f l =
    Prelude.foldr S.delete fList 
      $ (Prelude.map f [FlawLevel1, FlawLevel2, FlawLevel3])

setModelArchetype m =
  let 
    soc = m ^. sociability
    sub = m ^. submissiveness
    ont = m ^. ontology
    emp = m ^. empathy
    whatAttitude :: Maybe Sociability -> Maybe Submissiveness -> Maybe Ontology -> Maybe Empathy -> Attitude
    whatAttitude (Just so) (Just su) (Just o) (Just e) = Attitude so su o e
    whatAttitude _ _ _ _ = Neutral
  in 
    m & character . characterAlignment .~ Just (archetype $ whatAttitude soc sub ont emp)

setModelAttitude (Attitude soc sub ont emp) m = 
  ((( m & sociability .~ Just soc)
    & submissiveness .~ Just sub)
    & ontology .~ Just ont)
    & empathy .~ Just emp
setModelAttitude Neutral m = m

