module Twarog.Frontend.HamingjaStore
  (
   hamingjaOptions
  )
  where

-- | How starting hamingja points can be spent.
hamingjaOptions :: [(Int, String)] -- [(Cost, Description)]
hamingjaOptions =
  [ (2, "Gain 1 extra Character Role Skill")
  , (1, "Gain 1 extra Trained Skill")
  , (3, "Gain +1 mod to one chosen attribute")
  , (1, "Gain Heirloom")
  , (1, "Additional 2D6 oz of silver")
  , (2, "Additional 5D6 oz of silver")
  , (3, "Additional 8D6 oz of silver")
  , (1, "Gain an extra Talent")
  , (1, "If Bard, Ranger or Sorcerer, know 1 extra Weak spell")
  , (3, "You can freely pick your character's race")
  ]
