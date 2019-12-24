module Twarog.Backend.Gods
  ( -- * Gods representation
    God (..)
  , gods
  , LifeStance (..)
    -- ** Gods characteristics
  , godArchetype
  , godElement
  , godSex  
  , isSympathetic
    -- ** Polish names
  , godSkaudic
  ) where

import Twarog.Backend.Types (Sex(..))
import Twarog.Backend.Archetypes

data God = Vali
         | Heimdallr
         | Tyr
         | Vidarr
         | Saga
         | Thor
         | Baldr
         | Jord
         | Njordr
         | Forseti
         | Freyja
         | Sol
         | Freyr
         | Odinn
         | Skadi
         | Mani
         | Hodr
         | Austr
         | Loki
         | Surtr
         | Bolthorn
         | Idunn
         | Hel
         | Fjorgyn
         | Borr
         | Natt
         | Aegir
         | Skuld
         | Verdandi
         | Ymir
         | Urdr
         | Audhumbla
         deriving (Ord, Show, Eq, Enum)

data Element = Fire
             | Air
             | Water
             | Earth
             | Spirit
             deriving (Show, Eq)

-- | Get list of all sympathetic gods
gods :: [God]
gods = filter isSympathetic $ enumFrom (toEnum 0)

isSympathetic :: God -> Bool
isSympathetic god = god < Austr

godElement :: God -> Element
godElement = \case 
  Vali      -> Fire
  Heimdallr -> Spirit
  Tyr       -> Air
  Vidarr    -> Earth
  Saga      -> Air
  Thor      -> Fire
  Baldr     -> Fire
  Jord      -> Earth
  Njordr    -> Water
  Forseti   -> Air
  Freyja    -> Water
  Sol       -> Fire
  Freyr     -> Earth
  Odinn     -> Air
  Skadi     -> Water
  Mani      -> Water
  Hodr      -> Earth
  Austr     -> Fire
  Loki      -> Fire
  Surtr     -> Fire
  Bolthorn  -> Earth
  Idunn     -> Earth
  Hel       -> Earth
  Fjorgyn   -> Earth
  Borr      -> Air
  Natt      -> Air
  Aegir     -> Water
  Skuld     -> Water
  Verdandi  -> Water
  Ymir      -> Water
  Urdr      -> Water
  Audhumbla -> Spirit

-- | Bards can chose favourite deity of the same sex.
-- Heimdallr is an exception: it can be chosen both
-- by male and female PCs.
godSex :: God -> Sex
godSex = \case
  Vali      -> Male
  Heimdallr -> Non
  Tyr       -> Male
  Vidarr    -> Male
  Saga      -> Female
  Thor      -> Male
  Baldr     -> Male
  Jord      -> Female
  Njordr    -> Male
  Forseti   -> Female
  Freyja    -> Female
  Sol       -> Female
  Freyr     -> Male
  Odinn     -> Male
  Skadi     -> Female
  Mani      -> Female
  Hodr      -> Male
  Austr     -> Male
  Loki      -> Male
  Surtr     -> Male
  Bolthorn  -> Male
  Idunn     -> Female
  Hel       -> Female
  Fjorgyn   -> Female
  Borr      -> Female
  Natt      -> Female
  Aegir     -> Female
  Skuld     -> Female
  Verdandi  -> Female
  Ymir      -> Male
  Urdr      -> Female
  Audhumbla -> Female

godSkaudic :: God -> String
godSkaudic = \case
  Vali      -> "Dażbóg"
  Heimdallr -> "Kolęda"
  Tyr       -> "Ród"
  Vidarr    -> "Porwata"
  Saga      -> "Dodola"
  Thor      -> "Perun"
  Baldr     -> "Białobóg"
  Jord      -> "Matka Ziemia"
  Njordr    -> "Mokosz"
  Forseti   -> "Strzybóg"
  Freyja    -> "Łada"
  Sol       -> "Porewit"
  Freyr     -> "Veles"
  Odinn     -> "Swaróg"
  Skadi     -> "Dziewonna"
  Mani      -> "Jutrzenka"
  Hodr      -> "Czarnobóg"
  Austr     -> "Zorza"
  Loki      -> "Mylnica"
  Surtr     -> "Ogon"
  Bolthorn  -> "Zir"
  Idunn     -> "Kostroma"
  Hel       -> "Marzanna"
  Fjorgyn   -> "Rodzianica"
  Borr      -> "Bor"
  Natt      -> "Noc"
  Aegir     -> "Poświst"
  Skuld     -> "Dola"
  Verdandi  -> "Baba Jaga"
  Ymir      -> "Gol"
  Urdr      -> "Licho"
  Audhumbla -> "Ciszyna"

godArchetype :: God -> Archetype
godArchetype = \case
  Vali      -> Heraklean
  Heimdallr -> Kronic
  Tyr       -> Uranic
  Vidarr    -> Panic
  Saga      -> Athenic
  Thor      -> Zevsean
  Baldr     -> Apollonian
  Jord      -> Demeteric
  Njordr    -> Poseidonic
  Forseti   -> Aresean
  Freyja    -> Aphroditic
  Sol       -> Heliosean
  Freyr     -> Dionysian
  Odinn     -> Hermetic
  Skadi     -> Artemisian
  Mani      -> Selenic
  Hodr      -> Plutonic
  _         -> error "Unsympathetic deity"

data LifeStance = Religious God
                | Traditional
                deriving (Eq, Show)
