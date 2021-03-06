module Twarog.Backend.Talents
  ( 
  -- * Talents
    Talent (..)
  , talents
  -- ** Utils
  , availableTalents
  )
  where

import qualified Data.Set as S

import Twarog.Backend.Types
import Twarog.Backend.Races
import Twarog.Backend.Calendar
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
            | HerakleanTalent
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
            | ZevseanTalent
              -- ^ Requires Thrower
            | Aegirean
            deriving (Eq, Enum, Ord)

instance Show Talent where
  show Acrobatic = "Acrobatic"
  show Aggresive = "Aggresive"
  show AnimalFriend = "Animal Friend"
  show Arachnean = "Arachnean"
  show Argonautic = "Argonautic"
  show Ascetic = "Ascetic"
  show Athletic = "Athletic"
  show Bloodhound = "Bloodhound"
  show Calliopean = "Calliopean"
  show Careful = "Careful"
  show Cliopean = "Cliopean"
  show Courageous = "Courageous"
  show Craftsman = "Craftsman"
  show Curious = "Curious"
  show DartThrower = "Dart Thrower"
  show DeepBreather = "Deep Breather"
  show Dodger = "Dodger"
  show Durable = "Durable"
  show Empathic = "Empathic"
  show Enduring = "Enduring"
  show Eratorean = "Eratorean"
  show Euterpean = "Euterpean"
  show Fast = "Fast"
  show FastSleeper = "Fast Sleeper"
  show Favourite = "Favourite"
  show Fearless = "Fearless"
  show Fighter = "Fighter"
  show FistFighter = "Fist Fighter"
  show Focused = "Focused"
  show GoodReflexes = "Good Reflexes"
  show HawkEyed = "Hawk-Eyed"
  show Hephaestusean = "Hephaestusean"
  show HerakleanTalent = "Heraklean Talent"
  show Herbalist = "Herbalist"
  show Humble = "Humble"
  show Inquisitive = "Inquisitive"
  show Lancer = "Lancer"
  show LightFooted = "Light Footed"
  show LynxEyes = "Lynx Eyes"
  show Mariner = "Mariner"
  show Marked = "Marked"
  show Mechanic = "Mechanic"
  show Malpogomeanean = "Malpogomeanean"
  show Mermaid = "Mermaid"
  show Mule = "Mule"
  show Nimble = "Nimble"
  show Perseusean = "Perseusean"
  show Pietistic = "Pietistic"
  show Polyhymnian = "Polyhymnian"
  show Rider = "Rider"
  show Sensitive = "Sensitive"
  show Sharpshooter = "Sharpshooter"
  show Shooter = "Shooter"
  show Sirenean = "Sirenean"
  show Slinger = "Slinger"
  show SlowAgeing = "Slow Ageing"
  show SpearThrower = "Spear Thrower"
  show Springy = "Springy"
  show StrongBack = "Strong Back"
  show StrongGrip = "Strong Grip"
  show Survivor = "Survivor"
  show Swimmer = "Swimmer"
  show SwordDancer = "Sword Dancer"
  show Terpsichorean = "Terpsichorean"
  show Thalian = "Thalian"
  show Thrower = "Thrower"
  show Tough = "Tough"
  show Tracker = "Tracker"
  show TricksterTalent = "Trickster Talent"
  show Uranian = "Uranian"
  show WarmHands = "Warm Hands"
  show ZevseanTalent = "Zevsean"
  show Aegirean = "Aegirean"

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
    HerakleanTalent -> Fighter `elem` ts
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
    ZevseanTalent   -> Thrower `elem` ts
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
