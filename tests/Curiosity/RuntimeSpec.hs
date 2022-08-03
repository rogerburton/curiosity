module Curiosity.RuntimeSpec
  ( spec
  ) where

import           Curiosity.Data
import           Curiosity.Runtime
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Runtime" $ do
    it "Should boot." $ do
      -- TODO I don't like that this involves logging stuff.
      _ <- liftIO $ boot' emptyHask "/tmp/curiosity-test-xxx.log"
      1 `shouldBe` (1 :: Int)
