module Twarog.Backend.Modifier
  ( Modifier (..)
  )
  where

import Control.Lens

import Twarog.Backend.Talents
import Twarog.Backend.Types
import Twarog.Backend.Flaws
import Twarog.Backend.Skills
import Twarog.Backend.CharacterSheet
import Twarog.Backend.Character

class Modifier m where
  mod :: CharacterSheet -> m -> CharacterSheet

instance Modifier Talent where
  mod cr = \case
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
