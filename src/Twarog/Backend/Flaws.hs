module Twarog.Backend.Flaws
  ( Flaw (..)
  , FlawMod (..)
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
          | Lazy FlawLevel
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
          deriving (Eq, Show)

data FlawLevel = FlawLevel1
               | FlawLevel2
               | FlawLevel3
               deriving (Eq, Show, Enum)

data FlawMod = HPFlaw Mod
             | MoraleFlaw Mod
             | PerceptionFlaw Mod
             | MeleeFlaw Mod
             | MissileFlaw Mod
             | ChaFlaw Mod
             | CHAFlaw Mod
             | WILFlaw Mod
             | CONFlaw Mod
             | StrFlaw Mod
             | EncumbranceFlaw (Float -> Float)
             | ConFlaw Mod
             | SizeFlaw Mod
             | DEXFlaw Mod
             | ToughnessFlaw Mod
             | INTFlaw Mod
             | DVMEFlaw Mod
             | RuneLoreFlaw Mod
             | FortitudeFlaw Mod
             | OtherFlaw Note
             | MaximalAgeFlaw Float
             | ActingFlaw Mod
             | LyrePlayingFlaw Mod
             | FlutePlayingFlaw Mod
             | SingingFlaw Mod
             | SocialSkillsFlaw Mod
