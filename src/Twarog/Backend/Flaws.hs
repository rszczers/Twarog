module Twarog.Backend.Flaws
  ( Flaw (..)
  , FlawMod (..)
  , FlawLevel (..)
  , flawMod
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

flawMod :: Flaw -> [FlawMod]
flawMod = \case
  Alcoholic fl        -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You need to drink at least 1 justa of alcoholic " ++
                            "beverage every day. If you fail to drink what you " ++
                            "need you become sick. Your maximum age is reduced."
                         , MaximalAgeFlaw 0.9 
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You need to drink at least 2 justa of alcoholic " ++
                            "beverage every day. Your maximum age is reduced."
                         , MaximalAgeFlaw 0.8   
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You need to drink at least 3 justa of alcoholic " ++
                            "beverage every day. Your maximum age is reduced."
                         , MaximalAgeFlaw 0.6   
                         ]
  Annoying fl         -> case fl of
    FlawLevel1        -> [ ChaFlaw (\x -> x - 1)
                         , OtherFlaw $
                            "Your look, your voice, the way you dress or talk, " ++
                            "or something else is annoying to others."
                         ]   
    FlawLevel2        -> [ ChaFlaw (\x -> x - 2)
                         , OtherFlaw $
                            "Your look, your voice, the way you dress or talk, " ++
                            "or something else is annoying to others."
                         ]   
    FlawLevel3        -> [ ChaFlaw (\x -> x - 3)
                         , OtherFlaw $
                            "Your look, your voice, the way you dress or talk, " ++
                            "or something else is annoying to others."
                         ]   
  BadBack fl          -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "When Fumbling on any movement or combat skill " ++
                            "you lose 1 HP and suffer -1 mod to Str and Dex " ++
                            "for D6 hours. The HP lost is regained at the " ++
                            "end of the D6 hour period, and cannot be " ++
                            "healed by other means."
                         , EncumbranceFlaw (+ 0.25)
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "When Fumbling on any movement or combat skill " ++
                            "you lose D6 HP and suffer -1 mod to Str and Dex " ++
                            "for D6 days. The HP lost is regained at the " ++
                            "end of the D6 hour period, and cannot be " ++
                            "healed by other means."
                         , EncumbranceFlaw (+ 0.5)
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "When Fumbling on any movement or combat skill " ++
                            "you lose D8 HP and suffer -1 mod to Str and Dex " ++
                            "for D6 weeks. The HP lost is regained at the " ++
                            "end of the D6 hour period, and cannot be " ++
                            "healed by other means."
                         , EncumbranceFlaw (+ 0.75)
                         ]
  BadSight fl         -> case fl of
    FlawLevel1        -> [ PerceptionFlaw (\x -> x - 1)
                         , MeleeFlaw (\x -> x - 1)
                         , MissileFlaw (\x -> x - 1)
                         ]
    FlawLevel2        -> [ PerceptionFlaw (\x -> x - 2)
                         , MeleeFlaw (\x -> x - 2)
                         , MissileFlaw (\x -> x - 2)
                         ]
    FlawLevel3        -> [ PerceptionFlaw (\x -> x - 3)
                         , MeleeFlaw (\x -> x - 3)
                         , MissileFlaw (\x -> x - 3)
                         ]
  BadTempered fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You have problems controlling your anger, " ++
                            "and react violently to any and all " ++
                            "provocations."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You have problems controlling your anger, " ++
                            "and react violently to any and all " ++
                            "provocations."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You have problems controlling your anger, " ++
                            "and react violently to any and all " ++
                            "provocations."
                         ]
  ChronicPain fl      -> case fl of
    FlawLevel1        -> [ DEXFlaw (\x -> x - 1)
                         , OtherFlaw
                            "Something is constantly causing you pain."
                         ]
    FlawLevel2        -> [ DEXFlaw (\x -> x - 2)
                         , OtherFlaw
                            "Something is constantly causing you pain."
                         ]
    FlawLevel3        -> [ DEXFlaw (\x -> x - 3)
                         , OtherFlaw
                            "Something is constantly causing you pain."
                         ]
  Clumsy fl           -> case fl of
    FlawLevel1        -> [ OtherFlaw "You fumble more often." ]
    FlawLevel2        -> [ OtherFlaw "You fumble more often." ]
    FlawLevel3        -> [ OtherFlaw "You fumble more often." ]
  Coward fl           -> case fl of
    FlawLevel1        -> [ MoraleFlaw (\x -> x - 1) ]
    FlawLevel2        -> [ MoraleFlaw (\x -> x - 1) ]
    FlawLevel3        -> [ MoraleFlaw (\x -> x - 1) ]
  Delusional fl       -> case fl of
    FlawLevel1        -> [ INTFlaw (\x -> x - 1)
                         , OtherFlaw $ "You believe in something that is " ++
                             "simply not true"
                         ]
    FlawLevel2        -> [ INTFlaw (\x -> x - 2)
                         , OtherFlaw $ "You believe in something that is " ++
                             "simply not true"
                         ]
    FlawLevel3        -> [ INTFlaw (\x -> x - 3)
                         , OtherFlaw $ "You believe in something that is " ++
                             "simply not true"
                         ]
  Depressed fl        -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "To be allowed to get up in the morning " ++
                            "you need to test Wil against DD8. If " ++
                            "you fail, you stay in bed until the " ++
                            "next day. Every time you fail, " ++
                            "cast D20. On 1 you commit suicide " ++
                            "at Sunset."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "To be allowed to get up in the morning " ++
                            "you need to test Wil against DD10. If " ++
                            "you fail, you stay in bed until the " ++
                            "next day. Every time you fail, " ++
                            "cast D20. On 1 you commit suicide " ++
                            "at Sunset."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "To be allowed to get up in the morning " ++
                            "you need to test Wil against DD12. If " ++
                            "you fail, you stay in bed until the " ++
                            "next day. Every time you fail, " ++
                            "cast D20. On 1 you commit suicide " ++
                            "at Sunset."
                         ]
  Dislike fl          -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "There is something or someone, " ++
                            "an important individual, a species, " ++
                            "a race, a tribe or the like, " ++
                            "that you realy dislike and refuse to " ++
                            "cooperate with."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "There is something or someone, " ++
                            "an important individual, a species, " ++
                            "a race, a tribe or the like, " ++
                            "that you realy dislike and refuse to " ++
                            "cooperate with."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "There is something or someone, " ++
                            "an important individual, a species, " ++
                            "a race, a tribe or the like, " ++
                            "that you realy dislike and refuse to " ++
                            "cooperate with."
                         ]
  Dyslexia fl         -> case fl of
    FlawLevel1        -> [ OtherFlaw "You cannot read maps or runes."
                         , RuneLoreFlaw (\x -> x - 1)
                         ]
    FlawLevel2        -> [ OtherFlaw "You cannot read maps or runes."
                         , RuneLoreFlaw (\x -> x - 2)
                         ]
    FlawLevel3        -> [ OtherFlaw "You cannot read maps or runes."
                         , RuneLoreFlaw (\x -> x - 3)
                         ]
  Enemy fl            -> case fl of
    _                 -> [ OtherFlaw
                            "You've got a powerful enemy, looking to hurt you."
                         ]
  Fearful fl          -> case fl of
    FlawLevel1        -> [ MoraleFlaw (\x -> x - 1) ]
    FlawLevel2        -> [ MoraleFlaw (\x -> x - 2) ]
    FlawLevel3        -> [ MoraleFlaw (\x -> x - 3) ] 
  Frail fl            -> case fl of
    FlawLevel1        -> [ HPFlaw (\x -> x - 2) ]
    FlawLevel2        -> [ HPFlaw (\x -> x - 4) ]
    FlawLevel3        -> [ HPFlaw (\x -> x - 6) ]
  Gluttonous fl       -> case fl of
    FlawLevel1        -> [ SizeFlaw (+ 1)
                         , DEXFlaw (\x -> x - 1)
                         , CHAFlaw (\x -> x - 1)
                         ]
    FlawLevel2        -> [ SizeFlaw (+ 2)
                         , DEXFlaw (\x -> x - 2)
                         , CHAFlaw (\x -> x - 2)
                         ]
    FlawLevel3        -> [ SizeFlaw (+ 3)
                         , DEXFlaw (\x -> x - 3)
                         , CHAFlaw (\x -> x - 3)
                         ]
  Greedy fl           -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "Every time you have to opportunity to " ++
                            "profit from something you need resist " ++
                            "the strong temptation."   
                         ]   
    FlawLevel2        -> [ OtherFlaw $
                            "Every time you have to opportunity to " ++
                            "profit from something you need resist " ++
                            "the strong temptation."   
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "Every time you have to opportunity to " ++
                            "profit from something you need resist " ++
                            "the strong temptation."   
                         ]
  Gullible fl         -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You very easily believe in what others " ++
                            "tell you."  
                         ] 
    FlawLevel2        -> [ OtherFlaw $
                            "You very easily believe in what others " ++
                            "tell you."  
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You very easily believe in what others " ++
                            "tell you."  
                         ]
  Haemophilic fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "Whenever you get a bleeding wound, " ++
                            "it will never stop bleeding on its own."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "Whenever you get a bleeding wound, " ++
                            "it will never stop bleeding on its own."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "Whenever you get a bleeding wound, " ++
                            "it will never stop bleeding on its own."
                         ]
  Hypersexual fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You have a strong need to sleep with " ++
                            "individuals."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You have a strong need to sleep with " ++
                            "individuals."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You have a strong need to sleep with " ++
                            "individuals."
                         ]
  Jealous fl          -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You strongly dislike it when others are " ++
                            "better than you at something."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You strongly dislike it when others are " ++
                            "better than you at something."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You strongly dislike it when others are " ++
                            "better than you at something."
                         ]
  LawfulFlaw fl       -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You strongly resist breaking the law."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You strongly resist breaking the law."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You strongly resist breaking the law."
                         ]
  Lazy fl             -> case fl of
    FlawLevel1        -> [ OtherFlaw $ 
                            "You are lazy."
                         , WILFlaw (\x -> x - 1)   
                         ]
    FlawLevel2        -> [ OtherFlaw $ 
                            "You are lazy."
                         , WILFlaw (\x -> x - 2)   
                         ]
    FlawLevel3        -> [ OtherFlaw $ 
                            "You are lazy."
                         , WILFlaw (\x -> x - 3)   
                         ]
  Limp fl             -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You have a limp."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You have a limp."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You have a limp."
                         ]
  LowSelfEsteem fl    -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You find hard to do anything without doubts." 
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You find hard to do anything without doubts." 
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You find hard to do anything without doubts." 
                         ]
  Seasickness fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You get sick when in a vessel, even for " ++
                            "only a few minutes."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You get sick when in a vessel, even for " ++
                            "only a few minutes."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You get sick when in a vessel, even for " ++
                            "only a few minutes."
                         ]
  OverConfident fl    -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You always believe things are easier than " ++
                            "they actually are."
                         ]   
    FlawLevel2        -> [ OtherFlaw $
                            "You always believe things are easier than " ++
                            "they actually are."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You always believe things are easier than " ++
                            "they actually are."
                         ]
  Paranoid fl         -> case fl of
    _                 -> [ OtherFlaw $
                            "You tend to interpret others and what they " ++
                            "do as a threat to you and what you do, " ++
                            "even if they are not."
                         ]
  Parasite fl         -> case fl of
    _                 -> [ OtherFlaw $
                            "You feed on others and what others do. " ++
                            "You are unable to create anything yourself, " ++
                            "do any manual labour or think morally in " ++
                            "relation to others and their belongings."
                         ]
  Philia fl           -> case fl of
    _                 -> [ OtherFlaw $
                            "You have an irrational attraction to something."
                         ]
  Phobia fl           -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You have an irrational fear of something."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You have an irrational fear of something."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You have an irrational fear of something."
                         ]
  PhysicalDefect fl   -> case fl of
    FlawLevel1        -> [ CHAFlaw (\x -> x - 1)
                         , OtherFlaw $
                            "You have a very visible physical defect " ++
                            "that makes you easy to remember and identify."
                         ]
    FlawLevel2        -> [ CHAFlaw (\x -> x - 1)
                         , OtherFlaw $
                            "You have a very visible physical defect " ++
                            "that makes you easy to remember and identify."
                         ]
    FlawLevel3        -> [ CHAFlaw (\x -> x - 1)
                         , OtherFlaw $
                            "You have a very visible physical defect " ++
                            "that makes you easy to remember and identify."
                         ]
  PhysicalWeakness fl -> case fl of
    FlawLevel1        -> [ DVMEFlaw (\x -> x - 1)
                         , OtherFlaw $
                            "You have a visible physical weakness that " ++
                            "makes you easy to injure."
                         ]
    FlawLevel2        -> [ DVMEFlaw (\x -> x - 2)
                         , OtherFlaw $
                            "You have a visible physical weakness that " ++
                            "makes you easy to injure."
                         ]
    FlawLevel3        -> [ DVMEFlaw (\x -> x - 3)
                         , OtherFlaw $
                            "You have a visible physical weakness that " ++
                            "makes you easy to injure."
                         ]
  PoorHearing fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw "Your hearing is poor." ]
    FlawLevel2        -> [ OtherFlaw "Your hearing is poor." ]
    FlawLevel3        -> [ OtherFlaw "Your hearing is poor." ]
  Secret fl           -> case fl of
    _                 -> [ OtherFlaw $
                            "You have a secret that you really don't want " ++ 
                            "others to know."
                         ]
  SelfHating fl       -> case fl of
    _                 -> [ OtherFlaw $
                            "You have an exceptionally negative view " ++
                            "on yourself. You constantly excuse yourself " ++
                            "and even your own existence, and are always " ++
                            "willing to take the blame for whatever goes " ++
                            "wrong."
                         ]
  Selfish fl          -> case fl of
    _                 -> [ OtherFlaw $
                            "You find hard to put the needs of others " ++
                            "before your own."
                         ]
  Selfless fl         -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "You find hard to put your needs before " ++
                            "needs of others."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "You find hard to put your needs before " ++
                            "needs of others."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "You find hard to put your needs before " ++
                            "needs of others."
                         ]
  Sickly fl           -> case fl of
    FlawLevel1        -> [ OtherFlaw "You have poor health"
                         , CONFlaw (\x -> x - 1)
                         ]
    FlawLevel2        -> [ OtherFlaw "You have poor health"
                         , CONFlaw (\x -> x - 1)
                         ]
    FlawLevel3        -> [ OtherFlaw "You have poor health"
                         , CONFlaw (\x -> x - 1)
                         ]
  ShortLived fl       -> case fl of
    FlawLevel1        -> [ MaximalAgeFlaw 0.8 ]
    FlawLevel2        -> [ MaximalAgeFlaw 0.6 ]
    FlawLevel3        -> [ MaximalAgeFlaw 0.4 ]
  Shy fl              -> case fl of
    FlawLevel1        -> [ ActingFlaw (\x -> x - 1)
                         , LyrePlayingFlaw (\x -> x - 1)
                         , FlutePlayingFlaw (\x -> x - 1)
                         , SingingFlaw (\x -> x - 1)
                         , SocialSkillsFlaw (\x -> x - 1)
                         , OtherFlaw "You're a shy person."
                         ]
    FlawLevel2        -> [ ActingFlaw (\x -> x - 2)
                         , LyrePlayingFlaw (\x -> x - 2)
                         , FlutePlayingFlaw (\x -> x - 2)
                         , SingingFlaw (\x -> x - 2)
                         , SocialSkillsFlaw (\x -> x - 2)
                         , OtherFlaw "You're a shy person."
                         ]
    FlawLevel3        -> [ ActingFlaw (\x -> x - 3)
                         , LyrePlayingFlaw (\x -> x - 3)
                         , FlutePlayingFlaw (\x -> x - 3)
                         , SingingFlaw (\x -> x - 3)
                         , SocialSkillsFlaw (\x -> x - 3)
                         , OtherFlaw "You're a shy person."
                         ]
  SlaveMinded fl      -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                             "You have no initiative, and become " ++
                             "afraid and confused unless you have " ++
                             "someone to lead you."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                             "You have no initiative, and become " ++
                             "afraid and confused unless you have " ++
                             "someone to lead you."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                             "You have no initiative, and become " ++
                             "afraid and confused unless you have " ++
                             "someone to lead you."
                         ]
  Stubborn fl         -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                             "You always want your own way."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                             "You always want your own way."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                             "You always want your own way."
                         ]
  Stuttering fl       -> case fl of
    FlawLevel1        -> [ ActingFlaw (\x -> x - 1)
                         , SocialSkillsFlaw (\x -> x - 1)
                         ]
    FlawLevel2        -> [ ActingFlaw (\x -> x - 2)
                         , SocialSkillsFlaw (\x -> x - 2)
                         ]
    FlawLevel3        -> [ ActingFlaw (\x -> x - 3)
                         , SocialSkillsFlaw (\x -> x - 3)
                         ]
  Unlucky fl          -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "If something bad happens to character in the " ++
                            "player party, that's most likely you."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "If something bad happens to character in the " ++
                            "player party, that's most likely you."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "If something bad happens to character in the " ++
                            "player party, that's most likely you."
                         ]
  Vulnerable fl       -> case fl of
    _                 -> [ ToughnessFlaw (\x -> x - 1) ]
  WeakMinded fl       -> case fl of
    FlawLevel1        -> [ FortitudeFlaw (\x -> x - 1) ]
    FlawLevel2        -> [ FortitudeFlaw (\x -> x - 2) ]
    FlawLevel3        -> [ FortitudeFlaw (\x -> x - 3) ]
  Whiny fl            -> case fl of
    FlawLevel1        -> [ OtherFlaw $
                            "Whenever you face a problem/danger you " ++
                            "most likely whine and complain."
                         ]
    FlawLevel2        -> [ OtherFlaw $
                            "Whenever you face a problem/danger you " ++
                            "most likely whine and complain."
                         ]
    FlawLevel3        -> [ OtherFlaw $
                            "Whenever you face a problem/danger you " ++
                            "most likely whine and complain."
                         ]
