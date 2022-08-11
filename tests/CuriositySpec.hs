module CuriositySpec
  ( spec
  ) where

import qualified Curiosity.Data.User           as User
import qualified Data.Aeson                    as Aeson
import qualified Data.Text.Encoding            as T
import           System.FilePath                ( (</>) )
import           Test.Hspec


--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "UserProfile JSON parser" $ do
    let go (filename, username) = it ("Parses " <> filename) $ do
          content <- readFile $ "data/" </> filename
          let Right (result :: User.UserProfile) =
                Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          User._userProfileDisplayName result `shouldBe` username
    mapM_
      go
      [ ("alice.json"  , "Alice")
      , ("bob-0.json"  , "Bob")
      , ("bob-1.json"  , "Bob")
      , ("bob-2.json"  , "Bob")
      , ("charlie.json", "Charlie")
      ]
