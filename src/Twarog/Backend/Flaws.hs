module Twarog.Backend.Flaws
  ( Flaw (..)
  , flaws
  , flaws'
  , FlawLevel (..)
  , flawTree
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
          deriving (Ord, Eq)

flawTree :: [(FlawLevel -> Flaw, [FlawLevel])]
flawTree = 
  [ ( Alcoholic, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Annoying, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( BadBack, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( BadSight, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( BadTempered, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( ChronicPain, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Clumsy, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Coward, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Delusional, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Depressed, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Dislike, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Dyslexia, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Enemy, [FlawLevel1] )
  , ( Fearful, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Frail, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Gluttonous, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Greedy, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Gullible, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Haemophilic, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Hypersexual, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Jealous, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( LawfulFlaw, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( LazyFlaw, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Limp, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( LowSelfEsteem, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Seasickness, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( OverConfident, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Paranoid, [FlawLevel1] )
  , ( Parasite, [FlawLevel1] )
  , ( Philia, [FlawLevel1] )
  , ( Phobia, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( PhysicalDefect, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( PhysicalWeakness, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( PoorHearing, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Secret, [FlawLevel1] )
  , ( SelfHating, [FlawLevel1] )
  , ( Selfish, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Selfless, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Sickly, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( ShortLived, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Shy, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( SlaveMinded, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Stubborn, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Stuttering, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Unlucky, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Vulnerable, [FlawLevel1] )
  , ( WeakMinded, [FlawLevel1, FlawLevel2, FlawLevel3] )
  , ( Whiny, [FlawLevel1, FlawLevel2, FlawLevel3] )
  ]

instance Eq (FlawLevel -> Flaw) where
  f == g = f FlawLevel1 == g FlawLevel1

instance Ord (FlawLevel -> Flaw) where
  f <= g = f FlawLevel1 <= g FlawLevel1

instance Show Flaw where
  show = \case
    Alcoholic lvl -> "Alcoholic" ++ " " ++ show lvl
    Annoying lvl -> "Annoying" ++ " " ++ show lvl
    BadBack lvl -> "Bad Back" ++ " " ++ show lvl
    BadSight lvl -> "Bad Sight" ++ " " ++ show lvl
    BadTempered lvl -> "Bad Tempered" ++ " " ++ show lvl
    ChronicPain lvl -> "Chronic Pain" ++ " " ++ show lvl
    Clumsy lvl -> "Clumsy" ++ " " ++ show lvl
    Coward lvl -> "Coward" ++ " " ++ show lvl
    Delusional lvl -> "Delusional" ++ " " ++ show lvl
    Depressed lvl -> "Depressed" ++ " " ++ show lvl
    Dislike lvl -> "Dislike" ++ " " ++ show lvl
    Dyslexia lvl -> "Dyslexia" ++ " " ++ show lvl
    Enemy lvl -> "Enemy" ++ " " ++ show lvl
    Fearful lvl -> "Fearful" ++ " " ++ show lvl
    Frail lvl -> "Frail" ++ " " ++ show lvl
    Gluttonous lvl -> "Gluttonous" ++ " " ++ show lvl
    Greedy lvl -> "Greedy" ++ " " ++ show lvl
    Gullible lvl -> "Gullible" ++ " " ++ show lvl
    Haemophilic lvl -> "Haemophilic" ++ " " ++ show lvl
    Hypersexual lvl -> "Hypersexual"  ++ " " ++ show lvl
    Jealous lvl -> "Jealous"  ++ " " ++ show lvl
    LawfulFlaw lvl -> "Lawful"  ++ " " ++ show lvl
    LazyFlaw lvl -> "LazyFlaw"  ++ " " ++ show lvl
    Limp lvl -> "Limp"  ++ " " ++ show lvl
    LowSelfEsteem lvl -> "Low Self-Esteem"  ++ " " ++ show lvl
    Seasickness lvl -> "Seasickness"  ++ " " ++ show lvl
    OverConfident lvl -> "Over Confident"  ++ " " ++ show lvl
    Paranoid lvl -> "Paranoid"  ++ " " ++ show lvl
    Parasite lvl -> "Parasite"  ++ " " ++ show lvl
    Philia lvl -> "Philia"  ++ " " ++ show lvl
    Phobia lvl -> "Phobia"  ++ " " ++ show lvl
    PhysicalDefect lvl -> "Physical Defect"  ++ " " ++ show lvl
    PhysicalWeakness lvl -> "Physical Weakness"  ++ " " ++ show lvl
    PoorHearing lvl -> "Poor Hearing"  ++ " " ++ show lvl
    Secret lvl -> "Secret"  ++ " " ++ show lvl
    SelfHating lvl -> "Self Hating"  ++ " " ++ show lvl
    Selfish lvl -> "Selfish"  ++ " " ++ show lvl
    Selfless lvl -> "Selfless"  ++ " " ++ show lvl
    Sickly lvl -> "Sickly"  ++ " " ++ show lvl
    ShortLived lvl -> "Short Lived"  ++ " " ++ show lvl
    Shy lvl -> "Shy"  ++ " " ++ show lvl
    SlaveMinded lvl -> "Slave Minded"  ++ " " ++ show lvl
    Stubborn lvl -> "Stubborn"  ++ " " ++ show lvl
    Stuttering lvl -> "Stuttering"  ++ " " ++ show lvl
    Unlucky lvl -> "Unlucky"  ++ " " ++ show lvl
    Vulnerable lvl -> "Vulnerable"  ++ " " ++ show lvl
    WeakMinded lvl -> "Weak Minded"  ++ " " ++ show lvl
    Whiny lvl -> "Whiny"  ++ " " ++ show lvl

data FlawLevel = FlawLevel1
               | FlawLevel2
               | FlawLevel3
               deriving (Ord, Eq, Enum)

instance Show FlawLevel where
  show FlawLevel1 = "Level 1"
  show FlawLevel2 = "Level 2"
  show FlawLevel3 = "Level 3"

-- | Well, not all combinations are available 
instance Enum Flaw where
  toEnum = \case
    0 -> Alcoholic FlawLevel1
    1 -> Alcoholic FlawLevel2
    2 -> Alcoholic FlawLevel3
    3 -> Annoying FlawLevel1
    4 -> Annoying FlawLevel2
    5 -> Annoying FlawLevel3
    6 -> BadBack FlawLevel1
    7 -> BadBack FlawLevel2
    8 -> BadBack FlawLevel3
    9 -> BadSight FlawLevel1
    10 -> BadSight FlawLevel2
    11 -> BadSight FlawLevel3
    12 -> BadTempered FlawLevel1
    13 -> BadTempered FlawLevel2
    14 -> BadTempered FlawLevel3
    15 -> ChronicPain FlawLevel1
    16 -> ChronicPain FlawLevel2
    17 -> ChronicPain FlawLevel3
    18 -> Clumsy FlawLevel1
    19 -> Clumsy FlawLevel2
    20 -> Clumsy FlawLevel3
    21 -> Coward FlawLevel1
    22 -> Coward FlawLevel2
    23 -> Coward FlawLevel3
    24 -> Delusional FlawLevel1
    25 -> Delusional FlawLevel2
    26 -> Delusional FlawLevel3
    27 -> Depressed FlawLevel1
    28 -> Depressed FlawLevel2
    29 -> Depressed FlawLevel3
    30 -> Dislike FlawLevel1
    31 -> Dislike FlawLevel2
    32 -> Dislike FlawLevel3
    33 -> Dyslexia FlawLevel1
    34 -> Dyslexia FlawLevel2
    35 -> Dyslexia FlawLevel3
    36 -> Enemy FlawLevel1
    37 -> Fearful FlawLevel1
    38 -> Fearful FlawLevel2
    39 -> Fearful FlawLevel3
    40 -> Frail FlawLevel1
    41 -> Frail FlawLevel2
    42 -> Frail FlawLevel3
    43 -> Gluttonous FlawLevel1
    44 -> Gluttonous FlawLevel2
    45 -> Gluttonous FlawLevel3
    46 -> Greedy FlawLevel1
    47 -> Greedy FlawLevel2
    48 -> Greedy FlawLevel3
    49 -> Gullible FlawLevel1
    50 -> Gullible FlawLevel2
    51 -> Gullible FlawLevel3
    52 -> Haemophilic FlawLevel1
    53 -> Haemophilic FlawLevel2
    54 -> Haemophilic FlawLevel3
    55 -> Hypersexual FlawLevel1
    56 -> Hypersexual FlawLevel2
    57 -> Hypersexual FlawLevel3
    58 -> Jealous FlawLevel1
    59 -> Jealous FlawLevel2
    60 -> Jealous FlawLevel3
    61 -> LawfulFlaw FlawLevel1
    62 -> LawfulFlaw FlawLevel2
    63 -> LawfulFlaw FlawLevel3
    64 -> LazyFlaw FlawLevel1
    65 -> LazyFlaw FlawLevel2
    66 -> LazyFlaw FlawLevel3
    67 -> Limp FlawLevel1
    68 -> Limp FlawLevel2
    69 -> Limp FlawLevel3
    70 -> LowSelfEsteem FlawLevel1
    71 -> LowSelfEsteem FlawLevel2
    72 -> LowSelfEsteem FlawLevel3
    73 -> Seasickness FlawLevel1
    74 -> Seasickness FlawLevel2
    75 -> Seasickness FlawLevel3
    76 -> OverConfident FlawLevel1
    77 -> OverConfident FlawLevel2
    78 -> OverConfident FlawLevel3
    79 -> Paranoid FlawLevel1
    80 -> Parasite FlawLevel1
    81 -> Philia FlawLevel1
    82 -> Phobia FlawLevel1
    83 -> Phobia FlawLevel2
    84 -> Phobia FlawLevel3
    85 -> PhysicalDefect FlawLevel1
    86 -> PhysicalDefect FlawLevel2
    87 -> PhysicalDefect FlawLevel3
    88 -> PhysicalWeakness FlawLevel1
    89 -> PhysicalWeakness FlawLevel2
    90 -> PhysicalWeakness FlawLevel3
    91 -> PoorHearing FlawLevel1
    92 -> PoorHearing FlawLevel2
    93 -> PoorHearing FlawLevel3
    94 -> Secret FlawLevel1
    95 -> SelfHating FlawLevel1
    96 -> Selfish FlawLevel1
    97 -> Selfish FlawLevel2
    98 -> Selfish FlawLevel3
    99 -> Selfless FlawLevel1
    100 -> Selfless FlawLevel2
    101 -> Selfless FlawLevel3
    102 -> Sickly FlawLevel1
    103 -> Sickly FlawLevel2
    104 -> Sickly FlawLevel3
    105 -> ShortLived FlawLevel1
    106 -> ShortLived FlawLevel2
    107 -> ShortLived FlawLevel3
    108 -> Shy FlawLevel1
    109 -> Shy FlawLevel2
    110 -> Shy FlawLevel3
    111 -> SlaveMinded FlawLevel1
    112 -> SlaveMinded FlawLevel2
    113 -> SlaveMinded FlawLevel3
    114 -> Stubborn FlawLevel1
    115 -> Stubborn FlawLevel2
    116 -> Stubborn FlawLevel3
    117 -> Stuttering FlawLevel1
    118 -> Stuttering FlawLevel2
    119 -> Stuttering FlawLevel3
    120 -> Unlucky FlawLevel1
    121 -> Unlucky FlawLevel2
    122 -> Unlucky FlawLevel3
    123 -> Vulnerable FlawLevel1
    124 -> WeakMinded FlawLevel1
    125 -> WeakMinded FlawLevel2
    126 -> WeakMinded FlawLevel3
    127 -> Whiny FlawLevel1
    128 -> Whiny FlawLevel2
    129 -> Whiny FlawLevel3
  fromEnum = \case
    Alcoholic FlawLevel1 -> 0 
    Alcoholic FlawLevel2 -> 1 
    Alcoholic FlawLevel3 -> 2 
    Annoying FlawLevel1 -> 3 
    Annoying FlawLevel2 -> 4 
    Annoying FlawLevel3 -> 5 
    BadBack FlawLevel1 -> 6 
    BadBack FlawLevel2 -> 7 
    BadBack FlawLevel3 -> 8 
    BadSight FlawLevel1 -> 9 
    BadSight FlawLevel2 -> 10 
    BadSight FlawLevel3 -> 11 
    BadTempered FlawLevel1 -> 12 
    BadTempered FlawLevel2 -> 13 
    BadTempered FlawLevel3 -> 14 
    ChronicPain FlawLevel1 -> 15 
    ChronicPain FlawLevel2 -> 16 
    ChronicPain FlawLevel3 -> 17 
    Clumsy FlawLevel1 -> 18 
    Clumsy FlawLevel2 -> 19 
    Clumsy FlawLevel3 -> 20 
    Coward FlawLevel1 -> 21 
    Coward FlawLevel2 -> 22 
    Coward FlawLevel3 -> 23 
    Delusional FlawLevel1 -> 24 
    Delusional FlawLevel2 -> 25 
    Delusional FlawLevel3 -> 26 
    Depressed FlawLevel1 -> 27 
    Depressed FlawLevel2 -> 28 
    Depressed FlawLevel3 -> 29 
    Dislike FlawLevel1 -> 30 
    Dislike FlawLevel2 -> 31 
    Dislike FlawLevel3 -> 32 
    Dyslexia FlawLevel1 -> 33 
    Dyslexia FlawLevel2 -> 34 
    Dyslexia FlawLevel3 -> 35 
    Enemy FlawLevel1 -> 36 
    Fearful FlawLevel1 -> 37 
    Fearful FlawLevel2 -> 38 
    Fearful FlawLevel3 -> 39 
    Frail FlawLevel1 -> 40 
    Frail FlawLevel2 -> 41 
    Frail FlawLevel3 -> 42 
    Gluttonous FlawLevel1 -> 43 
    Gluttonous FlawLevel2 -> 44 
    Gluttonous FlawLevel3 -> 45 
    Greedy FlawLevel1 -> 46 
    Greedy FlawLevel2 -> 47 
    Greedy FlawLevel3 -> 48 
    Gullible FlawLevel1 -> 49 
    Gullible FlawLevel2 -> 50 
    Gullible FlawLevel3 -> 51 
    Haemophilic FlawLevel1 -> 52 
    Haemophilic FlawLevel2 -> 53 
    Haemophilic FlawLevel3 -> 54 
    Hypersexual FlawLevel1 -> 55 
    Hypersexual FlawLevel2 -> 56 
    Hypersexual FlawLevel3 -> 57 
    Jealous FlawLevel1 -> 58 
    Jealous FlawLevel2 -> 59 
    Jealous FlawLevel3 -> 60 
    LawfulFlaw FlawLevel1 -> 61 
    LawfulFlaw FlawLevel2 -> 62 
    LawfulFlaw FlawLevel3 -> 63 
    LazyFlaw FlawLevel1 -> 64 
    LazyFlaw FlawLevel2 -> 65 
    LazyFlaw FlawLevel3 -> 66 
    Limp FlawLevel1 -> 67 
    Limp FlawLevel2 -> 68 
    Limp FlawLevel3 -> 69 
    LowSelfEsteem FlawLevel1 -> 70 
    LowSelfEsteem FlawLevel2 -> 71 
    LowSelfEsteem FlawLevel3 -> 72 
    Seasickness FlawLevel1 -> 73 
    Seasickness FlawLevel2 -> 74 
    Seasickness FlawLevel3 -> 75 
    OverConfident FlawLevel1 -> 76 
    OverConfident FlawLevel2 -> 77 
    OverConfident FlawLevel3 -> 78 
    Paranoid FlawLevel1 -> 79 
    Parasite FlawLevel1 -> 80 
    Philia FlawLevel1 -> 81 
    Phobia FlawLevel1 -> 82 
    Phobia FlawLevel2 -> 83 
    Phobia FlawLevel3 -> 84 
    PhysicalDefect FlawLevel1 -> 85 
    PhysicalDefect FlawLevel2 -> 86 
    PhysicalDefect FlawLevel3 -> 87 
    PhysicalWeakness FlawLevel1 -> 88 
    PhysicalWeakness FlawLevel2 -> 89 
    PhysicalWeakness FlawLevel3 -> 90 
    PoorHearing FlawLevel1 -> 91 
    PoorHearing FlawLevel2 -> 92 
    PoorHearing FlawLevel3 -> 93 
    Secret FlawLevel1 -> 94 
    SelfHating FlawLevel1 -> 95 
    Selfish FlawLevel1 -> 96 
    Selfish FlawLevel2 -> 97 
    Selfish FlawLevel3 -> 98 
    Selfless FlawLevel1 -> 99 
    Selfless FlawLevel2 -> 100 
    Selfless FlawLevel3 -> 101 
    Sickly FlawLevel1 -> 102 
    Sickly FlawLevel2 -> 103 
    Sickly FlawLevel3 -> 104 
    ShortLived FlawLevel1 -> 105 
    ShortLived FlawLevel2 -> 106 
    ShortLived FlawLevel3 -> 107 
    Shy FlawLevel1 -> 108 
    Shy FlawLevel2 -> 109 
    Shy FlawLevel3 -> 110 
    SlaveMinded FlawLevel1 -> 111 
    SlaveMinded FlawLevel2 -> 112 
    SlaveMinded FlawLevel3 -> 113 
    Stubborn FlawLevel1 -> 114 
    Stubborn FlawLevel2 -> 115 
    Stubborn FlawLevel3 -> 116 
    Stuttering FlawLevel1 -> 117 
    Stuttering FlawLevel2 -> 118 
    Stuttering FlawLevel3 -> 119 
    Unlucky FlawLevel1 -> 120 
    Unlucky FlawLevel2 -> 121 
    Unlucky FlawLevel3 -> 122 
    Vulnerable FlawLevel1 -> 123 
    WeakMinded FlawLevel1 -> 124 
    WeakMinded FlawLevel2 -> 125 
    WeakMinded FlawLevel3 -> 126 
    Whiny FlawLevel1 -> 127 
    Whiny FlawLevel2 -> 128 
    Whiny FlawLevel3 -> 129 

instance Bounded Flaw where
  minBound = Alcoholic FlawLevel1
  maxBound = Whiny FlawLevel3 

-- | Lists all _available_ flaws in lexicographic order
flaws :: [Flaw]
flaws = [minBound .. maxBound]

flaws' :: [FlawLevel -> Flaw]
flaws' = [ Alcoholic
         , Annoying
         , BadBack
         , BadSight
         , BadTempered
         , ChronicPain
         , Clumsy
         , Coward
         , Delusional
         , Depressed
         , Dislike
         , Dyslexia
         , Enemy
         , Fearful
         , Frail
         , Gluttonous
         , Greedy
         , Gullible
         , Haemophilic
         , Hypersexual
         , Jealous
         , LawfulFlaw
         , LazyFlaw
         , Limp
         , LowSelfEsteem
         , Seasickness
         , OverConfident
         , Paranoid
         , Parasite
         , Philia
         , Phobia
         , PhysicalDefect
         , PhysicalWeakness
         , PoorHearing
         , Secret
         , SelfHating
         , Selfish
         , Selfless
         , Sickly
         , ShortLived
         , Shy
         , SlaveMinded
         , Stubborn
         , Stuttering
         , Unlucky
         , Vulnerable
         , WeakMinded
         , Whiny
         ]
