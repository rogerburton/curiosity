module Curiosity.RuntimeSpec
  ( spec
  ) where

import           Curiosity.Data
import           Curiosity.Data.User
import           Curiosity.Runtime
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Runtime" $ do
    it "Should boot." $ do
      -- TODO I don't like that this involves logging stuff.
      runtime <- boot' emptyHask "/tmp/curiosity-test-xxx-1.log"
      st <- readFullStmDbInHaskFromRuntime runtime
      st `shouldBe` emptyHask

  describe "Runtime / Users" $ do
    it "Empty state returns no user." $ do
      -- TODO powerdown shoud be called at the end of the previous test,
      -- otherwise a "resource busy (file is locked)" will occur.
      -- Which is indeed related to logging, which I'd like to remove from
      -- these tests / runtime.
      runtime <- boot' emptyHask "/tmp/curiosity-test-xxx-2.log"
      let db = _rDb runtime
      muser <- atomically $ selectUserById db "USER-1"

      muser `shouldBe` Nothing

    it ("The first user ID is " <> show (unUserId firstUserId) <> ".") $ do
      runtime <- boot' emptyHask "/tmp/curiosity-test-xxx-2-1.log"
      let db = _rDb runtime
      id <- atomically $ generateUserId db

      id `shouldBe` firstUserId

    it "Adding a user, returns a user." $ do
      runtime <- boot' emptyHask "/tmp/curiosity-test-xxx-3.log"
      let db = _rDb runtime
          input = Signup "alice" "secret" "alice@example.com" True
      Right uid <- atomically $ createUser db input
      Just profile <- atomically $ selectUserById db uid

      uid `shouldBe` "USER-1"
      _userProfileId profile `shouldBe` uid

    it "Blocklisted username cannot be used." $ do
      runtime <- boot' emptyHask "/tmp/curiosity-test-xxx-4.log"
      let db = _rDb runtime
          input = Signup "smartcoop" "secret" "smartcoop@example.com" True
      muser <- atomically $ createUser db input

      muser `shouldBe` Left UsernameBlocked
