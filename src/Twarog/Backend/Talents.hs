module Twarog.Backend.Talents
  ( 
  -- * Talents
    Talent (..)
  , talents
  -- ** Talent modifiers 
  , TalentMod (..)
  , talentMod
  -- ** Utils
  , availableTalents
  )
  where

import Twarog.Backend.Types
import Twarog.Backend.Races
import Twarog.Backend.Encumbrance

data Talent = Acrobatic
            | Aggresive
            | AnimalFriend
              -- ^ Requires Empathic
            | Arachnean
              -- ^ Requires Strong Grip
            | Argonautic
              -- ^ Requires Mariner
            | Ascetic
            | Athletic
            | Bloodhound
              -- ^ Requires Tracker
            | Calliopean
              -- ^ Requires Polyhymnian
            | Careful
              -- ^ Requires Focused
            | Cliopean
              -- ^ Requires Curious
            | Courageous
            | Craftsman
              -- ^ Requires Nimble
            | Curious
            | DartThrower
              -- ^ Requires Thrower
            | DeepBreather
              -- ^ Requires Swimmer
            | Dodger
              -- ^ Requires Good Reflexes
            | Durable
            | Empathic
            | Enduring
            | Eratorean
              -- ^ Requires Polyhymnian
            | Euterpean
              -- ^ Requires Polyhymnian
            | Fast
              -- ^ Requires Athletic
            | FastSleeper
            | Favourite
              -- ^ Requires Marked
            | Fearless
              -- ^ Requires Courageous
            | Fighter
            | FistFighter
              -- ^ Requires Fighter
            | Focused
            | GoodReflexes
            | HawkEyed
            | Hephaestusean
              -- ^ Requires Craftsman
            | Heraklean
              -- ^ Requires Fighter
            | Herbalist
            | Humble
            | Inquisitive
              -- ^ Requires Cliopean
            | Lancer
              -- ^ Requires Fighter
            | LightFooted
            | LynxEyes
            | Mariner
            | Marked
            | Mechanic
              -- ^ Requires Nimble
            | Malpogomeanean
            | Mermaid
              -- ^ Requires Swimmer
            | Mule
              -- ^ Requires Strong Back
            | Nimble
            | Perseusean
              -- ^ Requires Good Reflexes
            | Pietistic
              -- ^ Requires Humble
            | Polyhymnian
            | Rider
              -- ^ Requires Animal Friend
            | Sensitive
              -- ^ Requires Empathic
            | Sharpshooter
              -- ^ Requires Shooter
            | Shooter
              -- ^ Requires Focused
            | Sirenean
              -- ^ Requires Polyhymnian
            | Slinger
              -- ^ Requires Thrower
            | SlowAgeing
            | SpearThrower
              -- ^ Requires Thrower
            | Springy
            | StrongBack
            | StrongGrip
            | Survivor
            | Swimmer
            | SwordDancer
              -- ^ Requires Fighter
            | Terpsichorean
              -- ^ Requires Acrobatic
            | Thalian
            | Thrower
            | Tough
            | Tracker
              -- ^ Requires Focused
            | TricksterTalent
              -- ^ Requires Nimble
            | Uranian
            | WarmHands
            | Zevsean
              -- ^ Requires Thrower
            | Aegirean
            deriving (Eq, Show, Enum)

-- | Check if PC has prerequisited talents
isTalentAvailable :: [Talent] -> Race -> Talent -> Bool
isTalentAvailable ts r t = if t `elem` ts then False 
  else case t of
    AnimalFriend    -> Empathic `elem` ts
    Arachnean       -> StrongGrip `elem` ts
    Argonautic      -> Mariner `elem` ts
    Bloodhound      -> Tracker `elem` ts
    Calliopean      -> Polyhymnian `elem` ts
    Careful         -> Focused `elem` ts
    Cliopean        -> Curious `elem` ts
    Craftsman       -> Nimble `elem` ts
    DartThrower     -> Thrower `elem` ts
    DeepBreather    -> Swimmer `elem` ts
    Dodger          -> GoodReflexes `elem` ts
    Eratorean       -> Polyhymnian `elem` ts
    Euterpean       -> Polyhymnian `elem` ts
    Fast            -> Athletic `elem` ts
    Favourite       -> Marked `elem` ts
    Fearless        -> Courageous `elem` ts
    FistFighter     -> Fighter `elem` ts
    Hephaestusean   -> Craftsman `elem` ts
    Heraklean       -> Fighter `elem` ts
    Inquisitive     -> Cliopean `elem` ts
    Lancer          -> Fighter `elem` ts
    Mechanic        -> Nimble `elem` ts
    Mermaid         -> Swimmer `elem` ts
    Mule            -> StrongBack `elem` ts
    Perseusean      -> GoodReflexes `elem` ts
    Pietistic       -> Humble `elem` ts
    Rider           -> AnimalFriend  `elem` ts
    Sensitive       -> Empathic `elem` ts
    Sharpshooter    -> Shooter `elem` ts
    Shooter         -> Focused `elem` ts
    Sirenean        -> Polyhymnian `elem` ts
    Slinger         -> Thrower `elem` ts
    SpearThrower    -> Thrower `elem` ts
    SwordDancer     -> Fighter `elem` ts
    Terpsichorean   -> Acrobatic `elem` ts
    Tracker         -> Focused `elem` ts
    TricksterTalent -> Nimble `elem` ts
    Zevsean         -> Thrower `elem` ts
    SlowAgeing      -> r /= Elf
    _               -> True

-- | List all talents
talents :: [Talent]
talents = enumFrom (toEnum 0)

-- | Lists all achievable talents for given PC's talent list
availableTalents :: Race -> [Talent] -> [Talent]
availableTalents r ts = 
  let p = isTalentAvailable ts r 
   in filter p talents 

data TalentMod  = -- Skills based
                  AcrobaticsTalent Mod
                | ActingTalent Mod
                | AlchemyTalent Mod
                | ClimbingTalent Mod
                | CraftsTalent Mod
                | DancingTalent Mod
                | DodgingTalent Mod
                | FlutePlayingTalent Mod
                | ForagingTalent Mod
                | FortitudeTalent Mod
                | HealingTalent Mod
                | LyrePlayingTalent Mod
                | MechanicsTalent Mod
                | MeleeTalent Mod
                | MissileTalent Mod
                | NavigationTalent Mod
                | PerceptionTalent Mod
                | PoetryTalent Mod
                | ReligiousTraditionTalent Mod
                | RidingTalent Mod
                | RuneLoreTalent Mod
                | SeamanshipTalent Mod
                | SingingTalent Mod
                | SocialSkillsTalent Mod
                | StaminaTalent Mod
                | StealthTalent Mod
                | SwimmingTalent Mod
                | TempoTalent Mod
                | TrackingTalent Mod
                | TrickeryTalent Mod
                | WorldLoreTalent Mod
                  -- Others
                | InitiativeTalent Mod
                | MoraleTalent Mod
                | HeatTalent Mod
                | BlockTalent ShieldBlock
                | EncumbranceTalent EncumbranceMod
                | MaximumAgeTalent Mod
                | ElectricalTalent Mod
                | PhysicalTalent Mod
                | PoisonTalent Mod
                | FrightTalent Mod
                | OtherTalent Note
                deriving (Show)

talentMod :: Talent -> [TalentMod]
talentMod = \case
  Acrobatic       -> [ AcrobaticsTalent (+ 1)
                     , DancingTalent (+ 1)
                     ]
  Aggresive       -> [ InitiativeTalent (+ 1) ] 
  AnimalFriend    -> [ RidingTalent (+ 1) ]
  Arachnean       -> [ ClimbingTalent (+ 1) ]
  Argonautic      -> [ SeamanshipTalent (+ 1) ]
  Ascetic         -> [ OtherTalent
                         "Need only half the normally needed food and water" ]
  Athletic        -> [ TempoTalent (+ 5)
                     , OtherTalent "+1 speed when travelling"
                     ]
  Bloodhound      -> [ TrackingTalent (+ 1) ]
  Calliopean      -> [ PoetryTalent (+ 1) ]
  Careful         -> [ PerceptionTalent (+ 1)
                     , StealthTalent (+ 1) 
                     ]
  Cliopean       ->  [ WorldLoreTalent (+ 1) ]
  Courageous      -> [ MoraleTalent (+ 2) ]
  Craftsman       -> [ CraftsTalent (+ 1) ]
  Curious         -> [ WorldLoreTalent (+ 1) ]
  DartThrower     -> [ OtherTalent
                         "+1 Missile when throwing lead-weighted darts" ]
  DeepBreather    -> [ OtherTalent
                         "Spends only 1 SP/round when holding breath" ]
  Dodger          -> [ DodgingTalent (+ 1) ]
  Durable         -> [ HeatTalent (+ 1)
                     , StaminaTalent (+ 2) ]
  Empathic        -> [ SocialSkillsTalent (+ 1)
                     , HealingTalent (+ 1) 
                     ]
  Enduring        -> [ StaminaTalent (+ 2) ]
  Eratorean       -> [ LyrePlayingTalent (+ 1) ]
  Euterpean       -> [ FlutePlayingTalent (+ 1) ]
  Fast            -> [ TempoTalent (+ 5)
                     , OtherTalent
                         "+1 speed when travelling" 
                     ]
  FastSleeper     -> [ OtherTalent "Need only half the normally needed rest" ]
  Favourite       -> [ FortitudeTalent (+ 1) ]
  Fearless        -> [ MoraleTalent (+ 2) ]
  Fighter         -> [ MeleeTalent (+ 1) ]
  FistFighter     -> [ OtherTalent
                         "+1 Melee (when using unarmed combat/battle gloves)"
                     ]
  Focused         -> [ PerceptionTalent (+ 1) ]
  GoodReflexes    -> [ InitiativeTalent (+ 1)
                     , DodgingTalent (+ 1)
                     , BlockTalent (+ 5)
                     ]
  HawkEyed        -> [ OtherTalent $
                         "Negates all the mods a target gets to DV (MI) " ++
                         "from movement"
                     ]
  Hephaestusean   -> [ CraftsTalent (+ 1) ]
  Heraklean       -> [ OtherTalent "+1 Melee (when using concussion weapons)" ]
  Herbalist       -> [ AlchemyTalent (+ 1)
                     , ForagingTalent (+ 1)
                     ]
  Humble          -> [ ReligiousTraditionTalent (+ 1) ]
  Inquisitive     -> [ RuneLoreTalent (+ 1)
                     , AlchemyTalent (+ 1)
                     ]
  Lancer          -> [ OtherTalent "+ 1 Melee (when using spear weapons)" ]
  LightFooted     -> [ StealthTalent (+ 1) ]
  LynxEyes        -> [ OtherTalent $
                         "Night vision 30' (or +100' in total darkness, " ++
                         "if already got Night vision)"
                     ]        
  Mariner         -> [ SeamanshipTalent (+ 1) ]
  Marked          -> [ FortitudeTalent (+ 1) ]
  Mechanic        -> [ MechanicsTalent (+ 1) ]
  Malpogomeanean  -> [ ActingTalent (+ 1) ]
  Mermaid         -> [ SwimmingTalent (+ 1) ]
  Mule            -> [ EncumbranceTalent (\x -> x - 0.25) ]
  Nimble          -> [ MechanicsTalent (+ 1)
                     , TrickeryTalent (+ 1) 
                     ]
  Perseusean      -> [ BlockTalent (+ 0.05) ]
  Pietistic       -> [ ReligiousTraditionTalent (+ 1) ]
  Polyhymnian     -> [ FlutePlayingTalent (+ 1)
                     , LyrePlayingTalent (+ 1)
                     , PoetryTalent (+ 1)
                     , SingingTalent (+ 1)
                     ]
  Rider           -> [ RidingTalent (+ 1) ]
  Sensitive       -> [ RuneLoreTalent (+ 1) ]
  Sharpshooter    -> [ MissileTalent (+ 1) ]
  Shooter         -> [ MissileTalent (+ 1) ]
  Sirenean        -> [ SingingTalent (+ 1) ]
  Slinger         -> [ MissileTalent (+ 1) ]
  SlowAgeing      -> [ MaximumAgeTalent (\x -> x + (x `div` 5)) ]
  SpearThrower    -> [ MissileTalent (+ 1) ]
  Springy         -> [ AcrobaticsTalent (+ 1) ]
  StrongBack      -> [ EncumbranceTalent (\x -> x - 0.25) ]
  StrongGrip      -> [ ClimbingTalent (+ 1) ] 
  Survivor        -> [ SwimmingTalent (+ 1) ]
  Swimmer         -> [ SwimmingTalent (+ 1) ]
  SwordDancer     -> [ MeleeTalent (+ 1) ]
  Terpsichorean   -> [ DancingTalent (+ 1) ]
  Thalian         -> [ SocialSkillsTalent (+ 1)
                     , ActingTalent (+ 1)
                     ]
  Thrower         -> [ MissileTalent (+ 1) ]
  Tough           -> [ ElectricalTalent (+ 1)
                     , PhysicalTalent (+ 1)
                     , PoisonTalent (+ 1)
                     ]
  Tracker         -> [ TrackingTalent (+ 1)
                     , NavigationTalent (+ 1)
                     ]
  TricksterTalent -> [ TrickeryTalent (+ 1) ]
  Uranian         -> [ NavigationTalent (+ 1) ]
  WarmHands       -> [ HealingTalent (+ 1) ]
  Zevsean         -> [ MissileTalent (+ 1) ]
  Aegirean        -> [ FrightTalent (\x -> x - 1) ]

