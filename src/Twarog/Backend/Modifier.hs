module Twarog.Backend.Modifier
  ( Modifier (..)
  , mkCharacterSheet
  )
  where

import qualified Data.Text as T
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Twarog.Backend.Talents
import Twarog.Backend.Types
import Twarog.Backend.Races
import Twarog.Backend.Flaws
import Twarog.Backend.Encumbrance
import Twarog.Backend.Skills
import Twarog.Backend.Character
import Twarog.Backend.CharacterSheet

class Modifier c m where
  addMod :: c -> m -> c

instance Modifier NewCharacter Race where
  addMod nc@NewCharacter{..} r =
    let nc' = nc & (characterAttr %~ \a -> (do
                      sex <- _characterSex
                      fmap (raceAttrMod r sex) a)
                   )
        nc'' = foldr (\s -> characterSkills
                          . at s . _Just
                          . skillMod %~ raceSkillMod r s) nc' skills
     in case r of
       Dwarf ->
         nc'' & (characterOther %~ cons "Night Vision (can see 100' even in total darkness as if he had Ettin eyes)")
              . (characterOther %~ cons "Can learn Dwarf Spells")
              . (characterOther %~ cons "Can sense mechanical devices within 60' on 1-3 (D6)")
              . (characterOther %~ cons "Knows True North when Underground")
              . (characterFlaws %~ S.insert (Phobia FlawLevel1))
              . (characterOther %~ cons "Fear of Open Water")
              . (characterFlaws %~ S.insert (Greedy FlawLevel1))
              . (characterOther %~ cons "You are greedy")
              . (characterFlaws %~ S.insert (Dislike FlawLevel1))
              . (characterOther %~ cons "You dislike elves")
              . (characterFlaws %~ S.insert (Dislike FlawLevel2))
              . (characterOther %~ cons "You dislike orcs")
              . (let s = _characterAttr ^?! _Just . str
                     t' = 30 + (5 * (toModifier s 0))
                  in characterSkills . at Tempo . _Just . skillMod .~ t')
              . (characterOther %~ cons "Base Tempo is only 30'")
       Elf -> 
         nc'' & (characterOther %~ cons "Night Vision")
              . (characterFlaws %~ S.insert (Dislike FlawLevel1))
              . (characterOther %~ cons "You dislike dwarfs")
              . (characterFlaws %~ S.insert (Dislike FlawLevel2))
              . (characterOther %~ cons "You dislike orcs")
              . (characterOther %~ cons "Can learn Elf Spells and see Incorporeal Trolls, Nymphs and other spirits")
       Gnome ->               
         nc'' & (characterOther %~ cons "Night Vision")
              . (characterOther %~ cons "Can learn Gnome Spells")
              . (characterFlaws %~ S.insert (Phobia FlawLevel1))
              . (characterOther %~ cons "Fear of Open Water")
              . (let s = _characterAttr ^?! _Just . str
                     t' = 30 + (5 * (toModifier s 0))
                  in characterSkills . at Tempo . _Just . skillMod .~ t')
              . (characterOther %~ cons "Base Tempo is only 30'")
       HighMan -> nc''
       CommonMan -> nc''
       LesserMan -> nc''
       Halfling -> 
         nc'' & (characterOther %~ cons "Night Vision")
              . (characterFlaws %~ S.insert (Phobia FlawLevel1))
              . (characterOther %~ cons "Fear of Open Water")
              . (let s = _characterAttr ^?! _Just . str
                     t' = 30 + (5 * (toModifier s 0))
                  in characterSkills . at Tempo . _Just . skillMod .~ t')
              . (characterOther %~ cons "Base Tempo is only 30'")
       _ ->
         nc'' & (characterOther %~ cons "Night Vision (can see 100' even in total darkness as if he had Ettin eyes)")
              . (characterFlaws %~ S.insert (Phobia FlawLevel1))
              . (characterOther %~ cons "Fear of the Sun")
              . (characterFlaws %~ S.insert (Phobia FlawLevel1))
              . (characterOther %~ cons "Fear of Open Water")
              . (characterOther %~ cons "Can learn Orc Spells")
              . (characterFlaws %~ S.insert (Dislike FlawLevel2))
              . (characterOther %~ cons "You dislike elves")
              . (characterFlaws %~ S.insert (Dislike FlawLevel2))
              . (characterOther %~ cons "You dislike dwarfs")

instance Modifier NewCharacter Talent where
  addMod cr = undefined

instance Modifier NewCharacter Flaw where
  addMod cr = undefined

instance Modifier CharacterSheet Talent where
  addMod cr = \case
    Acrobatic ->
      cr & (sheetSkills . at Acrobatics . _Just . skillMod +~ 1)
         . (sheetSkills . at Dancing . _Just . skillMod +~ 1)
    Aggresive ->
      cr & sheetOther %~ cons "+1 Initiative"
    AnimalFriend ->
      cr & (sheetSkills . at Riding . _Just . skillMod +~ 1)
         . (sheetOther %~ cons "+2 for morale test against animals")
    Arachnean ->
      cr & (sheetSkills . at Climbing . _Just . skillMod +~ 1)
    Argonautic->
      cr & (sheetSkills . at Seamanship . _Just . skillMod +~ 1)
    Ascetic->
      cr & (sheetOther %~ cons "Need only half the normally needed food and water")
    Athletic ->
      cr & (sheetSkills . at Tempo . _Just . skillMod +~ 1)
         . (sheetOther %~ cons "+1 speed when traveling")
    Bloodhound->
      cr & (sheetSkills . at Tracking . _Just . skillMod +~ 1)
    Calliopean ->
      cr & (sheetSkills . at Poetry . _Just . skillMod +~ 1)
    Careful ->
      cr & (sheetSkills . at Perception . _Just . skillMod +~ 1)
         . (sheetSkills . at Stealth . _Just . skillMod +~ 1)
    Cliopean ->
      cr & (sheetSkills . at WorldLore . _Just . skillMod +~ 1)
    Courageous ->
      cr & (sheetOther %~ cons "+2 mod to all morale tests")
    Craftsman ->
      cr & (sheetSkills . at Crafts . _Just . skillMod +~ 1)
    Curious ->
      cr & (sheetSkills . at WorldLore . _Just . skillMod +~ 1)
    DartThrower ->
      cr & (sheetOther %~ cons "+1 Missile (when throwing lead-weighted darts")
    DeepBreather ->
      cr & (sheetOther %~ cons "Spends only 1 SP/round when holding his breath")
    Dodger ->
      cr & (sheetSkills . at Dodging . _Just . skillMod +~ 1)
    Durable ->
      cr & (sheetSkills . at Stamina . _Just . skillMod +~ 2)
         . (sheetToughness . toughnessHeat +~ 1)
    Empathic ->
      cr & (sheetSkills . at SocialSkills . _Just . skillMod +~ 1)
         . (sheetSkills . at Healing . _Just . skillMod +~ 1)
    Enduring ->
      cr & (sheetSkills . at Stamina . _Just . skillMod +~ 2)
    Eratorean ->
      cr & (sheetSkills . at LyrePlaying . _Just . skillMod +~ 1)
    Euterpean ->
      cr & (sheetSkills . at FlutePlaying . _Just . skillMod +~ 1)
    Fast ->
      cr & (sheetSkills . at Tempo . _Just . skillMod +~ 1)
         . (sheetOther %~ cons "+1 speed when traveling")
    FastSleeper ->
      cr & (sheetOther %~ cons "Need only half the normally needed rest")
    Favourite ->
      cr & (sheetSkills . at Fortitude . _Just . skillMod +~ 1)
    Fearless ->
      cr & (sheetOther %~ cons "+2 mod to all morale tests")
    Fighter ->
      cr & (sheetSkills . at Melee . _Just . skillMod +~ 1)
    FistFighter ->
      cr & (sheetOther %~
        cons "+1 Melee (when using unarmed combat/battle gloves)")
    Focused ->
      cr & (sheetSkills . at Perception . _Just . skillMod +~ 1)
    GoodReflexes ->
      cr & (sheetSkills . at Dodging . _Just . skillMod +~ 1)
         . (sheetOther %~ cons "+1 Initiative")
         . (sheetOther %~ cons "+5% MI Block when using shields")
    HawkEyed ->
      cr & (sheetOther %~
        cons "Negagtes all the mods a target gets to DV (MI) from movement")
    Hephaestusean ->
      cr & (sheetSkills . at Crafts . _Just . skillMod +~ 1)
    HerakleanTalent ->
      cr & (sheetOther %~ cons "+1 Melee (when using concussion weapons)")
    Herbalist ->
      cr & (sheetSkills . at Alchemy . _Just . skillMod +~ 1)
         . (sheetSkills . at Foraging . _Just . skillMod +~ 1)
    Humble ->
      cr & (sheetSkills . at ReligiousTradition . _Just . skillMod +~ 1)
    Inquisitive ->
      cr & (sheetSkills . at RuneLore . _Just . skillMod +~ 1)
         . (sheetSkills . at Alchemy . _Just . skillMod +~ 1)
    Lancer ->
      cr & (sheetOther %~ cons "+1 Melee (when using spear weapons)")
    LightFooted ->
      cr & (sheetSkills . at Stealth . _Just . skillMod +~ 1)
    LynxEyes ->
      cr & (sheetOther %~ cons
        "Night vision 30' (+100' in total darkness, if has Night vision)")
    Mariner ->
      cr & (sheetSkills . at Seamanship . _Just . skillMod +~ 1)
    Marked ->
      cr & (sheetSkills . at Fortitude . _Just . skillMod +~ 1)
    Mechanic ->
      cr & (sheetSkills . at Mechanics . _Just . skillMod +~ 1)
    Malpogomeanean ->
      cr & (sheetSkills . at Acting . _Just . skillMod +~ 1)
    Mermaid ->
      cr & (sheetSkills . at Swimming . _Just . skillMod +~ 1)
    Mule ->
      cr & sheetEncumbrance -~ (0.25 :: Float)
    Nimble ->
      cr & (sheetSkills . at Mechanics . _Just . skillMod +~ 1)
         . (sheetSkills . at Trickery . _Just . skillMod +~ 1)
    Perseusean ->
      cr & sheetOther %~ cons "+5% MI Block when using shields"
    Pietistic ->
      cr & (sheetSkills . at ReligiousTradition . _Just . skillMod +~ 1)
    Polyhymnian ->
      cr & (sheetSkills . at FlutePlaying . _Just . skillMod +~ 1)
         . (sheetSkills . at LyrePlaying . _Just . skillMod +~ 1)
         . (sheetSkills . at Poetry . _Just . skillMod +~ 1)
         . (sheetSkills . at Singing . _Just . skillMod +~ 1)
    Rider ->
      cr & (sheetSkills . at Riding . _Just . skillMod +~ 1)
    Sensitive ->
      cr & (sheetSkills . at RuneLore . _Just . skillMod +~ 1)
    Sharpshooter ->
      cr & sheetOther %~ cons "+1 Missile (when using a bow or crossbow)"
    Shooter ->
      cr & sheetOther %~ cons "+1 Missile (when using a bow or crossbow)"
    Sirenean ->
      cr & (sheetSkills . at Singing . _Just . skillMod +~ 1)
    Slinger ->
      cr & sheetOther %~ cons "+1 Missile (when using slings)"
    SlowAgeing -> case cr ^. sheetMaxAge of
      Immortal -> cr
      Mortal age ->
        cr & sheetMaxAge . _Mortal %~ (floor . (* 1.2) . fromIntegral)
    SpearThrower ->
      cr & sheetOther %~ cons "+1 Missile (when throwing spears)"
    Springy ->
      cr & (sheetSkills . at Acrobatics . _Just . skillMod +~ 1)
    StrongBack ->
      cr & sheetEncumbrance -~ (0.25 :: Float)
    StrongGrip ->
      cr & (sheetSkills . at Climbing . _Just . skillMod +~ 1)
    Survivor ->
      cr & (sheetSkills . at Foraging . _Just . skillMod +~ 1)
         . (sheetResistance . disease +~ 1)
         . (sheetToughness . toughnessCold +~ 1)
    Swimmer ->
      cr & (sheetSkills . at Swimming . _Just . skillMod +~ 1)
    SwordDancer ->
      cr & sheetOther %~ cons "+1 Melee (when using swords & daggers)"
    Terpsichorean ->
      cr & (sheetSkills . at Dancing . _Just . skillMod +~ 1)
    Thalian ->
      cr & (sheetSkills . at Acting . _Just . skillMod +~ 1)
         . (sheetSkills . at SocialSkills . _Just . skillMod +~ 1)
    Thrower ->
      cr & sheetOther %~ cons "+1 Melee (when throwing weapons)"
    Tough ->
      cr & (sheetToughness . toughnessElectricity +~ 1)
         . (sheetToughness . toughnessPhysical +~ 1)
         . (sheetResistance . poison +~ 1)
    Tracker ->
      cr & (sheetSkills . at Tracking . _Just . skillMod +~ 1)
         . (sheetSkills . at Navigation . _Just . skillMod +~ 1)
    TricksterTalent ->
      cr & (sheetSkills . at Trickery . _Just . skillMod +~ 1)
    Uranian ->
      cr & (sheetSkills . at Navigation . _Just . skillMod +~ 1)
    WarmHands ->
      cr & (sheetSkills . at Healing . _Just . skillMod +~ 1)
    ZevseanTalent ->
      cr & sheetOther %~ cons "+1 Melee (when throwing concussion weapons)"
    Aegirean ->
      cr & sheetFright -~ 1

instance Modifier CharacterSheet a => Modifier CharacterSheet (S.Set a) where
  addMod cr s = S.foldr (\t -> (flip addMod) t) cr s

instance Modifier CharacterSheet Flaw where
  addMod cr = \case
    Alcoholic l  -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You need to dring at least 1 justa of alcoholic beverage every day")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.9) . fromIntegral))
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You need to dring at least 2 justa of alcoholic beverage every day")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.8) . fromIntegral))
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You need to dring at least 3 justa of alcoholic beverage every day")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.6) . fromIntegral))
    Annoying l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "Your look, voice, the way you dress or talk, or something else is annoying to others")
           . (sheetAttributes . cha -~ 1)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "Your look, voice, the way you dress or talk, or something else is annoying to others")
           . (sheetAttributes . cha -~ 2)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "Your look, voice, the way you dress or talk, or something else is annoying to others")
           . (sheetAttributes . cha -~ 2)
    BadBack l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "When Fumbling on any movement or combat skill you lose 1 HP and suffer a -1 mod to Str and Dex for D6 hours.")
            . (sheetEncumbrance +~ (0.25 :: Float))
      FlawLevel2 ->
         cr & (sheetOther %~ cons "When Fumbling on any movement or combat skill you lose D6 HP and suffer a -1 mod to Str and Dex for D6 days.")
            . (sheetEncumbrance +~ (0.5 :: Float))
      FlawLevel3 ->
         cr & (sheetOther %~ cons "When Fumbling on any movement or combat skill you lose D8 HP and suffer a -1 mod to Str and Dex for D6 weeks.")
            . (sheetEncumbrance +~ (0.75 :: Float))
    BadSight l -> case l of
      FlawLevel1 ->
        cr & (sheetSkills . at Perception . _Just . skillMod -~ 1)
           . (sheetSkills . at Melee . _Just . skillMod -~ 1)
           . (sheetSkills . at Missile . _Just . skillMod -~ 1)
           . (sheetOther %~ cons "You suffer -1 mod to all skills involving sight")
      FlawLevel2 ->
        cr & (sheetSkills . at Perception . _Just . skillMod -~ 2)
           . (sheetSkills . at Melee . _Just . skillMod -~ 2)
           . (sheetSkills . at Missile . _Just . skillMod -~ 2)
           . (sheetOther %~ cons "You suffer -2 mod to all skills involving sight")
      FlawLevel3 ->
        cr & (sheetSkills . at Perception . _Just . skillMod -~ 3)
           . (sheetSkills . at Melee . _Just . skillMod -~ 3)
           . (sheetSkills . at Missile . _Just . skillMod -~ 3)
           . (sheetOther %~ cons "You suffer -3 mod to all skills involving sight")
    BadTempered l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You have problems controlling your anger, and react violently to any provocation; test Will against DD8 to control yourself")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You have problems controlling your anger, and react violently to any provocation; test Will against DD10 to control yourself")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You have problems controlling your anger, and react violently to any provocation; test Will against DD12 to control yourself")
    ChronicPain l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Something is constantly causing you pain")
           . (sheetAttributes . dex -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Something is constantly causing you pain")
           . (sheetAttributes . dex -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Something is constantly causing you pain")
           . (sheetAttributes . dex -~ 3)
    Clumsy l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Your fumbling range is increased by 1")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Your fumbling range is increased by 2")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Your fumbling range is increased by 3")
    Coward l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Your suffer a -1 mod to all Morale tests")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Your suffer a -2 mod to all Morale tests")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Your suffer a -3 mod to all Morale tests")
    Delusional l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You believe in something that is simply not true or does not actually exist, and deny all evidence proving you wrong")
           . (sheetAttributes . int -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You believe in something that is simply not true or does not actually exist, and deny all evidence proving you wrong")
           . (sheetAttributes . int -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You believe in something that is simply not true or does not actually exist, and deny all evidence proving you wrong")
           . (sheetAttributes . int -~ 3)
    Depressed l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "To be allowed to get up in the morning you need to test Wil against DD8. If you fail, you stay in bed until the next day. Every time you fail, cast a D20. On a 1 you commit suicide at Sunset.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "To be allowed to get up in the morning you need to test Wil against DD10. If you fail, you stay in bed until the next day. Every time you fail, cast a D20. On a 1 you commit suicide at Sunset.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "To be allowed to get up in the morning you need to test Wil against DD12. If you fail, you stay in bed until the next day. Every time you fail, cast a D20. On a 1 you commit suicide at Sunset.")
    Dislike l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "There is something or someone, an important individual, a species, a race, a tribe or the like, that you really dislike and refuse to cooperate with. If the something or someone you dislike dies you will automatically start to dislike something or someone else instead. Test your Wil against DD8 in order not to intentionally provoke what you dislike into attacking you.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "There is something or someone, an important individual, a species, a race, a tribe or the like, that you really dislike and refuse to cooperate with. If the something or someone you dislike dies you will automatically start to dislike something or someone else instead. Test your Wil against DD10 in order not to intentionally provoke what you dislike into attacking you.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "There is something or someone, an important individual, a species, a race, a tribe or the like, that you really dislike and refuse to cooperate with. If the something or someone you dislike dies you will automatically start to dislike something or someone else instead. Test your Wil against DD12 in order not to intentionally provoke what you dislike into attacking you.")
    Dyslexia l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You cannot read maps or runes")
            . (sheetSkills . at RuneLore . _Just . skillMod -~ 1)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You cannot read maps or runes")
            . (sheetSkills . at RuneLore . _Just . skillMod -~ 2)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You cannot read maps or runes")
            . (sheetSkills . at RuneLore . _Just . skillMod -~ 3)
    Enemy l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You have a powerful enemy looking to hurt you")
    Fearful l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You suffer a -1 mod to all Morale tests")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You suffer a -2 mod to all Morale tests")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You suffer a -3 mod to all Morale tests")
    Frail l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You suffer -2 maximum HP")
            . (sheetHealth -~ 2)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You suffer -4 maximum HP")
            . (sheetHealth -~ 4)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You suffer -6 maximum HP")
            . (sheetHealth -~ 6)
    Gluttonous l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You are overly fond of good food; your size is modified by +1; you get a -1 to your DEX and CHA")
            . (sheetAttributes . dex -~ 1)
            . (sheetAttributes . cha -~ 1)
            . (sheetSize +~ 1)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You are overly fond of good food; your size is modified by +2; you get a -2 to your DEX and CHA")
            . (sheetAttributes . dex -~ 2)
            . (sheetAttributes . cha -~ 2)
            . (sheetSize +~ 2)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You are overly fond of good food; your size is modified by +3; you get a -3 to your DEX and CHA")
            . (sheetAttributes . dex -~ 3)
            . (sheetAttributes . cha -~ 3)
            . (sheetSize +~ 3)
    Greedy l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "Every time you have the opportunity to profit from something, you need to test Wil against DD8 in order to resist the temptation")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "Every time you have the opportunity to profit from something, you need to test Wil against DD10 in order to resist the temptation")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "Every time you have the opportunity to profit from something, you need to test Wil against DD12 in order to resist the temptation")
    Gullible l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You very easily believe in what others tell you. Others who try to lie to or in other ways deceive you get a +1 mod to their Acting test.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You very easily believe in what others tell you. Others who try to lie to or in other ways deceive you get a +2 mod to their Acting test.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You very easily believe in what others tell you. Others who try to lie to or in other ways deceive you get a +3 mod to their Acting test.")
    Haemophilic l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "Whenever you get a bleeding wound, it will never stop bleeding on its own, and those who try to stop your bleeding suffer a -1 mod to their Healing test")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "Whenever you get a bleeding wound, it will never stop bleeding on its own, and those who try to stop your bleeding suffer a -2 mod to their Healing test")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "Whenever you get a bleeding wound, it will never stop bleeding on its own, and those who try to stop your bleeding suffer a -3 mod to their Healing test")
    Hypersexual l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You have a strong need to sleep with individuals of the opposite sex. You will have to test Wil against DD8 whenever you meet a potential sexual partner, and if you fail you will disregard all other obligations, and do everything you can in order to have sex with her/him.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You have a strong need to sleep with individuals of the opposite sex. You will have to test Wil against DD10 whenever you meet a potential sexual partner, and if you fail you will disregard all other obligations, and do everything you can in order to have sex with her/him.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You have a strong need to sleep with individuals of the opposite sex. You will have to test Wil against DD12 whenever you meet a potential sexual partner, and if you fail you will disregard all other obligations, and do everything you can in order to have sex with her/him.")
    Jealous l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You strongly dislike it when others are better than you at something - anything. When that happens test Wil against DD8, in order not to somehow insult or hurt them.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You strongly dislike it when others are better than you at something - anything. When that happens test Wil against DD10, in order not to somehow insult or hurt them.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You strongly dislike it when others are better than you at something - anything. When that happens test Wil against DD12, in order not to somehow insult or hurt them.")
    LawfulFlaw l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "In order to break the law you must every time test Wil against DD8.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "In order to break the law you must every time test Wil against DD10.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "In order to break the law you must every time test Wil against DD12.")
    LazyFlaw l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You are lazy, and try always to take the shortest and easiest way to the goal. Hard work is for others. You suffer a -1 to your WIL.")
            . (sheetAttributes . wil -~ 1)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You are lazy, and try always to take the shortest and easiest way to the goal. Hard work is for others. You suffer a -1 to your WIL.")
            . (sheetAttributes . wil -~ 2)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You are lazy, and try always to take the shortest and easiest way to the goal. Hard work is for others. You suffer a -1 to your WIL.")
            . (sheetAttributes . wil -~ 3)
    Limp l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "You have a limp and suffer a -5 penality to Tempo (and -1 speed when travelling). On the good side, you automatically gain the Marked Talent for free.")
            . (sheetSkills . at Tempo . _Just . skillMod -~ 5)
      FlawLevel2 ->
         cr & (sheetOther %~ cons "You have a limp and suffer a -10 penality to Tempo (and -2 speed when travelling). On the good side, you automatically gain the Marked Talent for free.")
            . (sheetSkills . at Tempo . _Just . skillMod -~ 10)
      FlawLevel3 ->
         cr & (sheetOther %~ cons "You have a limp and suffer a -15 penality to Tempo (and -3 speed when travelling). On the good side, you automatically gain the Marked Talent for free.")
            . (sheetSkills . at Tempo . _Just . skillMod -~ 15)
    LowSelfEsteem l -> case l of
      FlawLevel1 ->
         cr & (sheetOther %~ cons "To be allowed to test any skill against DD10 or more you first need to test Wil against DD8. If you fail, you fail to even give it a try. You are convinced you will fail anyhow.")
      FlawLevel2 ->
         cr & (sheetOther %~ cons "To be allowed to test any skill against DD10 or more you first need to test Wil against DD10. If you fail, you fail to even give it a try. You are convinced you will fail anyhow.")
      FlawLevel3 ->
         cr & (sheetOther %~ cons "To be allowed to test any skill against DD10 or more you first need to test Wil against DD12. If you fail, you fail to even give it a try. You are convinced you will fail anyhow.")
    Seasickness l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You get sick and suffer a -1 mod to all skills when in a vessel, even for only a few minutes.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You get sick and suffer a -2 mod to all skills when in a vessel, even for only a few minutes.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You get sick and suffer a -3 mod to all skills when in a vessel, even for only a few minutes.")
    OverConfident l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You always believe things are easier that they actually are. For skill test you might plan to take, the DD is estimated by you to be 2 lower than it actually is.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You always believe things are easier that they actually are. For skill test you might plan to take, the DD is estimated by you to be 4 lower than it actually is.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You always believe things are easier that they actually are. For skill test you might plan to take, the DD is estimated by you to be 6 lower than it actually is.")
    Paranoid l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You tend to interpret others and what they do as a threat to you and what you do, even if they are not.")
    Parasite l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You feed on others and what others do. You are unable to create anything yourself, do any manual labour or think morally in relation to others and their belongings. You think intelligence is the same as the ability to tell lies and to get away with crime.")
    Philia l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You have an irrational attraction to something.")
    Phobia l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You have an irrational fear of something and must test Wil against DD8 to be allowed to face what you fear, and also suffers a -1 mod to all skills when you do.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You have an irrational fear of something and must test Wil against DD10 to be allowed to face what you fear, and also suffers a -1 mod to all skills when you do.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You have an irrational fear of something and must test Wil against DD12 to be allowed to face what you fear, and also suffers a -1 mod to all skills when you do.")
    PhysicalDefect l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You have a very visible physical defect that gives you -1 to CHA and also makes you easy to remember and identify. On the good side, you automatically fain the Marked Talent for free.")
           . (sheetAttributes . cha -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You have a very visible physical defect that gives you -2 to CHA and also makes you easy to remember and identify. On the good side, you automatically fain the Marked Talent for free.")
           . (sheetAttributes . cha -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You have a very visible physical defect that gives you -3 to CHA and also makes you easy to remember and identify. On the good side, you automatically fain the Marked Talent for free.")
           . (sheetAttributes . cha -~ 3)
    PhysicalWeakness l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You have a visible physical weakness that maes you easy to injure in melee. You get a -1 to DV ME.")
           . (sheetCombatStats . dvMe -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You have a visible physical weakness that maes you easy to injure in melee. You get a -2 to DV ME.")
           . (sheetCombatStats . dvMe -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You have a visible physical weakness that maes you easy to injure in melee. You get a -3 to DV ME.")
           . (sheetCombatStats . dvMe -~ 3)
    PoorHearing l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You suffer a -1 mod to all skills involving hearing")
            . (sheetSkills . at Dancing . _Just . skillMod -~ 1)
            . (sheetSkills . at SocialSkills . _Just . skillMod -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You suffer a -2 mod to all skills involving hearing")
            . (sheetSkills . at Dancing . _Just . skillMod -~ 2)
            . (sheetSkills . at SocialSkills . _Just . skillMod -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You suffer a -3 mod to all skills involving hearing")
            . (sheetSkills . at Dancing . _Just . skillMod -~ 3)
            . (sheetSkills . at SocialSkills . _Just . skillMod -~ 3)
    Secret l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You have a secret that you really don't want others to know. If they find out, it will seriously influence their relationship to you - for the worse.")
    SelfHating l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You have an exceptionally negative view on yourself, possibly caused by brainwash, making you hate yourself and everyone who are like you. You constantly excuse yourself and even your own existence, and are always willing to take the blame for whatever goes wrong.")
    Selfish l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You must test Wil against DD8 in order to put the needs of the others before your own.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You must test Wil against DD10 in order to put the needs of the others before your own.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You must test Wil against DD12 in order to put the needs of the others before your own.")
    Selfless l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You must test Wil against DD8 in order to put (any of) your own needs before those of others.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You must test Wil against DD10 in order to put (any of) your own needs before those of others.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You must test Wil against DD12 in order to put (any of) your own needs before those of others.")
    Sickly l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Your health is rather poor. You suffer a -1 to your CON.")
           . (sheetAttributes . con -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Your health is rather poor. You suffer a -2 to your CON.")
           . (sheetAttributes . con -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Your health is rather poor. You suffer a -3 to your CON.")
           . (sheetAttributes . con -~ 3)
    ShortLived l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Your character's maximum age is only 80% of your race's normal")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.8) . fromIntegral))
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Your character's maximum age is only 80% of your race's normal")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.6) . fromIntegral))
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Your character's maximum age is only 80% of your race's normal")
           . (sheetMaxAge . _Mortal %~ (floor . (* 0.4) . fromIntegral))
    Shy l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You suffer a -1 mod to Acting, Music and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 1)
           . (sheetSkills . at Acting . _Just . skillMod -~ 1)
           . (sheetSkills . at LyrePlaying. _Just . skillMod -~ 1)
           . (sheetSkills . at Singing . _Just . skillMod -~ 1)
           . (sheetSkills . at FlutePlaying . _Just . skillMod -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You suffer a -2 mod to Acting, Music and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 2)
           . (sheetSkills . at Acting . _Just . skillMod -~ 2)
           . (sheetSkills . at LyrePlaying. _Just . skillMod -~ 2)
           . (sheetSkills . at Singing . _Just . skillMod -~ 2)
           . (sheetSkills . at FlutePlaying . _Just . skillMod -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You suffer a -3 mod to Acting, Music and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 3)
           . (sheetSkills . at Acting . _Just . skillMod -~ 3)
           . (sheetSkills . at LyrePlaying. _Just . skillMod -~ 3)
           . (sheetSkills . at Singing . _Just . skillMod -~ 3)
           . (sheetSkills . at FlutePlaying . _Just . skillMod -~ 3)
    SlaveMinded l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You have no initiative, and become afraid and confused unless you have smeone to lead you. You must test Wil against DD8 in order to do anything unless told you to do so.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You have no initiative, and become afraid and confused unless you have smeone to lead you. You must test Wil against DD10 in order to do anything unless told you to do so.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You have no initiative, and become afraid and confused unless you have smeone to lead you. You must test Wil against DD12 in order to do anything unless told you to do so.")
    Stubborn l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You always want your own way. If things does not go your way, you suffer a -1 mod to all skills for the rest of the day (until Sunrise), because you are so frustrated.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You always want your own way. If things does not go your way, you suffer a -2 mod to all skills for the rest of the day (until Sunrise), because you are so frustrated.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You always want your own way. If things does not go your way, you suffer a -3 mod to all skills for the rest of the day (until Sunrise), because you are so frustrated.")
    Stuttering l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You suffer a -1 mod to Acting, and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 1)
           . (sheetSkills . at Acting . _Just . skillMod -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You suffer a -2 mod to Acting, and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 2)
           . (sheetSkills . at Acting . _Just . skillMod -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You suffer a -3 mod to Acting, and Social Skills.")
           . (sheetSkills . at SocialSkills . _Just . skillMod -~ 3)
           . (sheetSkills . at Acting . _Just . skillMod -~ 3)
    Unlucky l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "If something bad happens to a randomly picked character in the player party, you are likely (1-3 in D6), to automatically be the one affected by this.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "If something bad happens to a randomly picked character in the player party, you are likely (1-4 in D6), to automatically be the one affected by this.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "If something bad happens to a randomly picked character in the player party, you are likely (1-5 in D6), to automatically be the one affected by this.")
    Vulnerable l -> case l of
      _ ->
        cr & (sheetOther %~ cons "You suffer a -1 to any one Toughness attribute. Effects are cumulative.")
    WeakMinded l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "You suffer -1 mod to Fortitude.")
           . (sheetSkills . at Fortitude . _Just . skillMod -~ 1)
      FlawLevel2 ->
        cr & (sheetOther %~ cons "You suffer -2 mod to Fortitude.")
           . (sheetSkills . at Fortitude . _Just . skillMod -~ 2)
      FlawLevel3 ->
        cr & (sheetOther %~ cons "You suffer -3 mod to Fortitude.")
           . (sheetSkills . at Fortitude . _Just . skillMod -~ 3)
    Whiny l -> case l of
      FlawLevel1 ->
        cr & (sheetOther %~ cons "Whenever you face a problem/danger you must test Wil against DD8 or you will be unable to do anything about it. Instead you will just whine and complain if the others don't solve your problems.")
      FlawLevel2 ->
        cr & (sheetOther %~ cons "Whenever you face a problem/danger you must test Wil against DD10 or you will be unable to do anything about it. Instead you will just whine and complain if the others don't solve your problems.")
      FlawLevel3 ->
        cr & (sheetOther %~ cons "Whenever you face a problem/danger you must test Wil against DD12 or you will be unable to do anything about it. Instead you will just whine and complain if the others don't solve your problems.")

instance Modifier CharacterSheet Encumbrance where
  addMod cr =
    let ms = typeSkill MovementSkill
        decr x =
          foldr (\s -> sheetSkills . at s . _Just . skillMod -~ x) cr ms
        -- ^ decrease all movements skill by x
     in \case
       LightLoad  -> cr
       MediumLoad -> decr 1
       HeavyLoad  -> decr 2
       AbsurdLoad -> decr 3

instance Modifier CharacterSheet Race where
  addMod cr@Player{..} r =
    let cr' = cr & (sheetAttributes %~ raceAttrMod r _sheetSex)
                 . (sheetResistance . disease %~ raceDiseaseMod r)
                 . (sheetResistance . poison %~ racePoisonMod r)
                 . (sheetToughness . toughnessCold %~ raceColdMod r)
                 . (sheetToughness . toughnessHeat %~ raceHeatMod r)
        cr'' = foldr (\s -> sheetSkills
                          . at s . _Just
                          . skillMod %~ raceSkillMod r s) cr' skills
     in case r of
       Dwarf ->
         cr'' & (sheetOther %~ cons "Night Vision (can see 100' even in total darkness as if he had Ettin eyes)")
              . (sheetOther %~ cons "Can learn Dwarf Spells")
              . (sheetOther %~ cons "Can sense mechanical devices within 60' on 1-3 (D6)")
              . (sheetOther %~ cons "Knows True North when Underground")
              . (sheetFlaws %~ S.insert (Phobia FlawLevel1))
              . (sheetOther %~ cons "Fear of Open Water")
              . (sheetFlaws %~ S.insert (Greedy FlawLevel1))
              . (sheetOther %~ cons "You are greedy")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel1))
              . (sheetOther %~ cons "You dislike elves")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel2))
              . (sheetOther %~ cons "You dislike orcs")
              . (let s = toModifier (_sheetAttributes ^. str) 0
                     t' = 30 + (5 * s)
                  in sheetSkills . at Tempo . _Just . skillMod .~ t')
              . (sheetOther %~ cons "Base Tempo is only 30'")
       Elf -> 
         cr'' & (sheetOther %~ cons "Night Vision")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel1))
              . (sheetOther %~ cons "You dislike dwarfs")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel2))
              . (sheetOther %~ cons "You dislike orcs")
              . (sheetOther %~ cons "Can learn Elf Spells and see Incorporeal Trolls, Nymphs and other spirits")
       Gnome ->               
         cr'' & (sheetOther %~ cons "Night Vision")
              . (sheetOther %~ cons "Can learn Gnome Spells")
              . (sheetFlaws %~ S.insert (Phobia FlawLevel1))
              . (sheetOther %~ cons "Fear of Open Water")
              . (let s = toModifier (_sheetAttributes ^. str) 0
                     t' = 30 + (5 * s)
                  in sheetSkills . at Tempo . _Just . skillMod .~ t')
              . (sheetOther %~ cons "Base Tempo is only 30'")
       HighMan -> cr''
       CommonMan -> cr''
       LesserMan -> cr''
       Halfling -> 
         cr'' & (sheetOther %~ cons "Night Vision")
              . (sheetFlaws %~ S.insert (Phobia FlawLevel1))
              . (sheetOther %~ cons "Fear of Open Water")
              . (let s = toModifier (_sheetAttributes ^. str) 0
                     t' = 30 + (5 * s)
                  in sheetSkills . at Tempo . _Just . skillMod .~ t')
              . (sheetOther %~ cons "Base Tempo is only 30'")
       _ ->
         cr'' & (sheetOther %~ cons "Night Vision (can see 100' even in total darkness as if he had Ettin eyes)")
              . (sheetFlaws %~ S.insert (Phobia FlawLevel1))
              . (sheetOther %~ cons "Fear of the Sun")
              . (sheetFlaws %~ S.insert (Phobia FlawLevel1))
              . (sheetOther %~ cons "Fear of Open Water")
              . (sheetOther %~ cons "Can learn Orc Spells")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel2))
              . (sheetOther %~ cons "You dislike elves")
              . (sheetFlaws %~ S.insert (Dislike FlawLevel2))
              . (sheetOther %~ cons "You dislike dwarfs")

-- | Generates character sheet once all necessary data is given
mkCharacterSheet :: NewCharacter -> Maybe CharacterSheet
mkCharacterSheet nc@NewCharacter{..} = do
  owner <- _characterOwner
  name <- _characterName
  attr <- _characterAttr
  race <- _characterRace
  birth <- _characterBirth
  alignment <- _characterAlignment
  lifeStance <- _characterLifeStance
  sex <- _characterSex
  hamingja <- _characterHamingja
  let flaws = _characterFlaws
  role <- _characterRole
  let skills = _characterSkills
  let talent = _characterTalent
  let _sheetPlayerName = owner
      _sheetCharName = name
      _sheetLevel = 1
      _sheetRole = role      
      _sheetAge = Mortal 1
      _sheetMaxAge = maximumAge race (attr ^. con) 
      _sheetRace = race
      _sheetHeight = raceHeight race sex
      _sheetSize = raceSizeMod race sex 0
      _sheetLifeStance = lifeStance
      _sheetAlignment = alignment
      _sheetHamingja = hamingja
      _sheetHealth = 
        hp (attr ^. con) (toModifier $ attr ^. str) _sheetSize role _sheetLevel
      _sheetSex = sex
      _sheetCombatStats = emptyCombatStats
      _sheetToughness = emptyToughness
      _sheetExperience = expToLvl race role sex 0
      _sheetEncumbrance = 0.0
      _sheetResistance = emptyResistance
      _sheetFlaws = flaws
      _sheetTalents = talent   
      _sheetSkills = skills
      _sheetEquipment = emptyEquipment
      _sheetAttributes = attr
      _sheetFright = 0
      _sheetOther = []
      initialSheet = Player{..}
  return $ initialSheet
--         `addMod` race -- it's taking for consideration at cration stage
         `addMod` talent 
         `addMod` flaws
         `addMod` (encumbrance $ _sheetEncumbrance)
