module Twarog.Backend.Flaws
  ( Flaw (..)
  , FlawLevel (..)
  )
  where

import Twarog.Backend.Types

data Flaw = Alcoholic FlawLevel
          | Annoying FlawLevel
          | BadBack FlawLevel
          | BadSight FlawLevel
          | BadTempered FlawLevel
          | ChronicPain FlawLevel
          | Clumsy FlawLevel
          | Coward FlawLevel
          | Delusional FlawLevel
          | Depressed FlawLevel
          | Dislike FlawLevel
          | Dyslexia FlawLevel
          | Enemy FlawLevel
          | Fearful FlawLevel
          | Frail FlawLevel
          | Gluttonous FlawLevel
          | Greedy FlawLevel
          | Gullible FlawLevel
          | Haemophilic FlawLevel
          | Hypersexual FlawLevel
          | Jealous FlawLevel
          | LawfulFlaw FlawLevel
          | LazyFlaw FlawLevel
          | Limp FlawLevel -- ^ Gives PC Marked Talent for free. 
          | LowSelfEsteem FlawLevel
          | Seasickness FlawLevel
          | OverConfident FlawLevel
          | Paranoid FlawLevel
          | Parasite FlawLevel
          | Philia FlawLevel
          | Phobia FlawLevel
          | PhysicalDefect FlawLevel
          | PhysicalWeakness FlawLevel
          | PoorHearing FlawLevel
          | Secret FlawLevel
          | SelfHating FlawLevel
          | Selfish FlawLevel
          | Selfless FlawLevel
          | Sickly FlawLevel
          | ShortLived FlawLevel
          | Shy FlawLevel
          | SlaveMinded FlawLevel
          | Stubborn FlawLevel
          | Stuttering FlawLevel
          | Unlucky FlawLevel
          | Vulnerable FlawLevel
          | WeakMinded FlawLevel
          | Whiny FlawLevel
          deriving (Ord, Eq, Show)

data FlawLevel = FlawLevel1
               | FlawLevel2
               | FlawLevel3
               deriving (Ord, Eq, Show, Enum)
