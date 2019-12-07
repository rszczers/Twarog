module Archetypes 
  ( Attitude
  , Sociability
  , Submissiveness
  , Ontology
  , Empathy
  , Trait
  , Archetype
  ) where

-- import System.Random

data Sociability = Introvert | Extravert
                 deriving (Show, Eq)

data Submissiveness = Lawful | Anarchic
                    deriving (Show, Eq)

data Ontology = Spiritual | Materialistic
              deriving (Show, Eq)

data Empathy = Compasionate | Cruel
             deriving (Show, Eq)

class Trait a where
  shadow :: a -> a

instance Trait Sociability where
  shadow Introvert = Extravert
  shadow _ = Introvert

instance Trait Submissiveness where
  shadow Lawful = Anarchic
  shadow _ = Lawful

instance Trait Ontology where
  shadow Spiritual = Materialistic
  shadow _ = Spiritual

instance Trait Empathy where
  shadow Compasionate = Cruel
  shadow _ = Compasionate

data Attitude = Attitude 
                  { _social :: Sociability
                  , _submissiveness :: Submissiveness
                  , _ontology :: Ontology
                  , _empathy :: Empathy
                  }
              | Neutral
              deriving (Show, Eq)

instance Trait Attitude where
  shadow (Attitude a b c d) =
    Attitude (shadow a) (shadow b) (shadow c) (shadow d)

data Archetype = Aphroditic
               | Apollonian
               | Aresean
               | Artemisian
               | Athenic
               | Demeteric
               | Dionysian
               | Heliosean
               | Heraklean
               | Hermetic
               | Kronic
               | Panic
               | Plutonic
               | Poseidonic
               | Selenic
               | Uranic
               | Zevsean
               deriving (Bounded, Enum, Show)

instance Trait Archetype where
  shadow = archetype . shadow . attitude

attitude :: Archetype -> Attitude
attitude Aphroditic = Attitude Introvert Anarchic Materialistic Compasionate
attitude Apollonian = Attitude Introvert Lawful Spiritual Compasionate
attitude Aresean    = Attitude Extravert Anarchic Spiritual Compasionate
attitude Artemisian = Attitude Introvert Lawful Materialistic Cruel
attitude Athenic    = Attitude Extravert Lawful Spiritual Compasionate
attitude Demeteric  = Attitude Extravert Lawful Materialistic Compasionate
attitude Dionysian  = Attitude Extravert Lawful Materialistic Cruel
attitude Heliosean  = Attitude Introvert Lawful Spiritual Cruel
attitude Heraklean  = Attitude Introvert Anarchic Spiritual Compasionate
attitude Hermetic   = Attitude Extravert Anarchic Spiritual Cruel
attitude Kronic     = Neutral
attitude Panic      = Attitude Extravert Anarchic Materialistic Compasionate
attitude Plutonic   = Attitude Extravert Anarchic Materialistic Cruel
attitude Poseidonic = Attitude Introvert Anarchic Materialistic Cruel
attitude Selenic    = Attitude Introvert Lawful Materialistic Compasionate
attitude Uranic     = Attitude Extravert Lawful Spiritual Compasionate
attitude Zevsean    = Attitude Introvert Anarchic Spiritual Cruel

archetype :: Attitude -> Archetype
archetype (Attitude Introvert Anarchic Materialistic Compasionate) = Aphroditic
archetype (Attitude Introvert Lawful Spiritual Compasionate) = Apollonian
archetype (Attitude Extravert Anarchic Spiritual Compasionate) = Aresean
archetype (Attitude Introvert Lawful Materialistic Cruel) = Artemisian
archetype (Attitude Extravert Lawful Spiritual Compasionate) = Athenic
archetype (Attitude Extravert Lawful Materialistic Compasionate) = Demeteric
archetype (Attitude Extravert Lawful Materialistic Cruel) = Dionysian
archetype (Attitude Introvert Lawful Spiritual Cruel) = Heliosean
archetype (Attitude Introvert Anarchic Spiritual Compasionate) = Heraklean
archetype (Attitude Extravert Anarchic Spiritual Cruel) = Hermetic
archetype Neutral = Kronic
archetype (Attitude Extravert Anarchic Materialistic Compasionate) = Panic
archetype (Attitude Extravert Anarchic Materialistic Cruel) = Plutonic
archetype (Attitude Introvert Anarchic Materialistic Cruel) = Poseidonic
archetype (Attitude Introvert Lawful Materialistic Compasionate) = Selenic
archetype (Attitude Extravert Lawful Spiritual Cruel) = Uranic
archetype (Attitude Introvert Anarchic Spiritual Cruel) = Zevsean

metric :: Attitude -> Attitude -> Int
metric Neutral (Attitude a b c d) = 4
metric a Neutral = metric Neutral a
metric (Attitude a1 b1 c1 d1) (Attitude a2 b2 c2 d2) =
  (* 2) . length $ filter id [a1 /= a2, b1 /= b2, c1 /= c2, d1 /= d2]

antagonist :: Archetype -> Archetype
antagonist = shadow 

-- instance Random Archetype where
--   random g = case randomR ( fromEnum (minBound :: Archetype)
--                           , fromEnum (maxBound :: Archetype)) g of
--                (r, g') -> (toEnum r, g')
--   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
--                       (r, g') -> (toEnum r, g')

-- getRandomArchetype :: IO Archetype
-- getRandomArchetype = randomRIO (minBound, maxBound)

