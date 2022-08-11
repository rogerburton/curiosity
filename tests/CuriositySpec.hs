module CuriositySpec
  ( spec
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Run                 as Run
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Options.Applicative           as A
import           Prelude                 hiding ( state )
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

  describe "Command-line interface parser" $ do
    let go (arguments, command) =
          it ("Parses '" <> T.unpack arguments <> "'") $ do
            let A.Success x =
                  A.execParserPure A.defaultPrefs Command.parserInfo
                    $ map T.unpack
                    $ words arguments
            x `shouldBe` command
    mapM_
      go
      [ ("init"      , Command.Init)
      , ("state"     , Command.State False)
      , ("state --hs", Command.State True)
      ]

  describe "Command-line interface execution" $ do
    let go (arguments, state) = it ("Runs '" <> T.unpack arguments <> "'") $ do
          stateFile <- pure "/tmp/curiosity-test-state.json"

          let A.Success command =
                A.execParserPure A.defaultPrefs Command.parserInfo
                  $ map T.unpack
                  $ words arguments

          (Run.run $ Command.CommandWithTarget
              command
              (Command.StateFileTarget stateFile)
              (Command.User $ User.UserName "alice")
            )
            `shouldThrow` (== ExitSuccess)

          content <- readFile stateFile
          let Right value = Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          value `shouldBe` state
    mapM_ go [("init", Data.emptyHask)]
