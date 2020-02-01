module Twarog.Frontend.CharGen
  ( genAttribute
  , genAttributes
  , genRace
  , genRace'
  , genMonth
  , genNames
  , genBirthday
  , genArchetype
  , genExp
  , genLvl
  , genSex
  , genSex'
  , genFlaw
  , genFlaws
  , genTalent
  , genTalents
  , genGod
  , genLifeStance
  , genLifeStance'
  , genCharacterRole
  , genInitCharacterSkills
  , genNewCharacter
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List ((\\))
import qualified Data.Text as T

import Twarog.Backend.Types
import Twarog.Backend.Races
import Twarog.Backend.Flaws
import Twarog.Backend.Talents
import Twarog.Backend.Calendar
import Twarog.Backend.Modifier
import Twarog.Backend.Skills
import Twarog.Backend.SkillMods
import Twarog.Backend.Character
import Twarog.Backend.Archetypes
import Twarog.Backend.Gods

import Twarog.Frontend.NameGen 
import Twarog.Frontend.DiceGen

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generate random valid attribute
genAttribute :: Gen Int
genAttribute = do
  x <- roll 3 D6
  y <- roll 3 D6
  let r = max (sum x) (sum y)
  if r > 3
  then return r
  else genAttribute

-- | Generate random attributes
genAttributes :: Gen Attributes
genAttributes = do
  cha <- genAttribute
  con <- genAttribute
  dex <- genAttribute
  int <- genAttribute
  str <- genAttribute
  wil <- genAttribute
  return $ Attributes cha con dex int str wil

-- | Generate random race
genRace :: Gen Race
genRace = Gen.choice $ Gen.constant <$> races

-- | Generate species according to distribution from the rulebook
genRace' :: Gen Race
genRace' = 
  let freqs race = case race of
        Dwarf -> 1
        Elf -> 1
        Gnome -> 0
        Halfling -> 2
        HighMan -> 1
        CommonMan -> 8
        LesserMan -> 8
        _ -> 0
      wRaces = (\r -> (freqs r, Gen.constant r)) <$> playableRaces
  in Gen.frequency wRaces

-- | Generate random month
genMonth :: Gen Month
genMonth = do
  let months' = Gen.constant <$> months
      wMonths = (1, Gen.constant Nyarsdagr) :
        ((27, ) <$> (tail . reverse $ months'))
  Gen.frequency wMonths

-- | Generate random birthday date
genBirthday :: Gen Birthday
genBirthday = do
  m <- genMonth
  if m == Nyarsdagr
  then return $ Birthday NewYearsDay Nyarsdagr
  else do
    cd <- Gen.int $ Range.constant 1 28
    return $ Birthday (CommonDay cd) m

-- | Generate random archetype
genArchetype :: Gen Archetype
genArchetype = Gen.choice $ Gen.constant <$> archetypes

-- | Generate random experience
genExp :: Gen XP
genExp = Gen.int $ Range.constant 1 40000

-- | Generate random character level
genLvl :: Gen Lvl
genLvl = Gen.int $ Range.linearFrom 10 1 100

genFlaw :: Gen Flaw
genFlaw = Gen.choice $ Gen.constant <$> flaws

genFlaws :: Gen (S.Set Flaw)
genFlaws = do
  let n = length flawTree 
  let genFlaw = Gen.choice $ Gen.constant <$> flaws'
  flaws <- Gen.set (Range.linear 0 n) genFlaw
  let s = S.size flaws
      genLevel = Gen.choice $ 
        Gen.constant <$> [FlawLevel1, FlawLevel2, FlawLevel3]
  levels <- Gen.list (Range.singleton s) genLevel
  return . S.fromList $ zipWith ($) (S.toList flaws) levels 

genNames :: T.Text -> Gen [(T.Text, Sex, NameRace)]
genNames str = do
  let possible_names = filterNames str
  names <- Gen.shuffle possible_names
  return names

genGod :: Gen God
genGod = Gen.choice $ Gen.constant <$> gods

genSex :: Gen Sex
genSex = Gen.choice $ Gen.constant <$> [Male, Female]

-- | Generate sex according to distribution from the rulebook
genSex' :: Race -> Gen Sex
genSex' = \case
  Dwarf     -> Gen.frequency [ (5, Gen.constant Male)
                             , (1, Gen.constant Female)
                             ]
  Elf       -> Gen.frequency [ (2, Gen.constant Male)
                             , (4, Gen.constant Female)
                             ]
  Gnome     -> Gen.frequency [ (5, Gen.constant Male)
                             , (1, Gen.constant Female)
                             ]
  Halfling  -> Gen.frequency [ (3, Gen.constant Male)
                             , (3, Gen.constant Female)
                             ]
  CommonMan -> Gen.frequency [ (3, Gen.constant Male)
                             , (3, Gen.constant Female)
                             ]
  LesserMan -> Gen.frequency [ (3, Gen.constant Male)
                             , (3, Gen.constant Female)
                             ]
  HighMan   -> Gen.frequency [ (3, Gen.constant Male)
                             , (3, Gen.constant Female)
                             ]

  _         -> Gen.frequency [ (5, Gen.constant Male)
                             , (1, Gen.constant Female)
                             ]

genLifeStance :: Gen LifeStance
genLifeStance = do
  god <- genGod
  Gen.choice $ Gen.constant <$> [Traditional, Religious god]

genLifeStance' :: Race -> Gen LifeStance
genLifeStance' race = 
  if isRaceTraditionalOnly race
  then return Traditional
  else do
    god <- genGod
    return $ Religious god

genCharacterRole :: Attributes -> [Talent]
                 -> LifeStance -> Race -> Sex -> Archetype
                 -> Gen CharacterRole
genCharacterRole attr ts ls r s a =
  let ar = Gen.constant <$> availableRoles attr ts ls r s a
   in Gen.choice ar

genTalent :: Race -> S.Set Talent -> Gen Talent
genTalent race ts = 
  let talents' = availableTalents race (S.toList ts)
   in Gen.choice $ Gen.constant <$> talents'

genTalents :: Race -> S.Set Flaw -> S.Set Talent -> Gen (S.Set Talent)
genTalents race fs ts = 
  let n = 1 + (S.size fs) `div` 2
  in if (S.size ts) < n
     then do
       t <- genTalent race ts
       genTalents race fs (S.insert t ts)
     else return ts

genInitCharacterSkills :: CharacterRole -> Sex -> Modifiers
                       -> Gen (M.Map Skill CharacterSkill)
genInitCharacterSkills cr sex mods@Modifiers{..} = do
  let crs = crSkills cr
      chs = Gen.choice $ Gen.constant <$> crs
  cr <- Gen.list (Range.singleton $ defaultCrSkillChoices cr) chs
  let trs = crs \\ cr
      utr = skills \\ crs
      trs' = zip trs $ repeat Trained
      crs' = zip crs $ repeat CharacterRoleSkill
      utr' = zip utr $ repeat Untrained
      allSkills = crs' ++ trs' ++ utr'
      mChars = (\(s, p) -> (s, CharacterSkill 0 p)) <$> allSkills
      initSkills = setPenalitySkill 
                 . (setBaseSkill mods)
                 $ M.fromList mChars
  return initSkills

genNewCharacter :: Gen NewCharacter
genNewCharacter = do
  race       <- genRace'
  lifestance <- genLifeStance' race
  sex        <- genSex' race 
  birth      <- genBirthday
  flaws      <- genFlaws
  talents    <- genTalents race flaws $ 
                  if isMarked birth
                  then S.singleton Marked
                  else S.empty
  attr       <- genAttributes
  arch       <- genArchetype
  role       <- genCharacterRole 
                  attr (S.toList talents) lifestance race sex arch
  skills     <- genInitCharacterSkills role sex (modifiers attr)
  let _characterOwner      = Nothing
      _characterName       = Nothing
      _characterRace       = Just race
      _characterBirth      = Just birth
      _characterAlignment  = Just arch
      _characterAttr       = Just attr
      _characterLifeStance = Just lifestance
      _characterSex        = Just sex
      _characterHamingja   = Just 3
      _characterFlaws      = flaws
      _characterRole       = Just role
      _characterSkills     = skills
      _characterTalent     = talents
      _characterOther      = []
      initChar             = NewCharacter{..}
  return $ initChar `addMod` race
