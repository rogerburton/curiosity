module Curiosity.CoreSpec
  ( spec
  ) where

import           Curiosity.Core
import           Curiosity.Data.User
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Core" $ do
    it ("The first user ID is " <> show (unUserId firstUserId) <> ".") $ do
      db <- atomically instantiateEmptyStmDb
      id <- atomically $ generateUserId db
      id `shouldBe` firstUserId
