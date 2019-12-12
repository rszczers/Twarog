module Update (updateModel) where

  import           Control.Lens
  import           Control.Monad.IO.Class
  import           Data.Maybe
  import           Miso
  import           Miso.String
  import           Model
  import           Twarog.Backend.Character
  import           Twarog.Backend.Talents
  
  -- | Updates model, optionally introduces side effects
  updateModel :: Msg -> Model -> Effect Msg Model
  updateModel (Name n) m = noEff $ m & character . characterName .~ (Just $ fromMisoString n)
  updateModel NoOp m = noEff m
  updateModel (RaceChecked r (Checked True)) m = noEff $ m & character . characterRace .~ r
  updateModel (TalentChecked t (Checked True)) m =  let currTalents = fromMaybe [] $ m ^. character . characterTalent
                                                    in
                                                        noEff (if Prelude.length currTalents < maxTalents
                                                          then m & character . characterTalent .~ Just (currTalents ++ [ t ])
                                                          else m )
  updateModel (TalentChecked r (Checked False)) m = let currTalents = fromMaybe [] $ m ^. character . characterTalent
                                                    in
                                                    noEff $ m & character . characterTalent .~
                                                      (Just $ Prelude.filter (/= r) currTalents)
  updateModel (ChangeStage s) m = noEff $ m & currentStage .~ s
  updateModel SayHelloWorld m = m <# do
    liftIO (putStrLn "Hello World") >> pure NoOp
  