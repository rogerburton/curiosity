module CuriositySpec
  ( spec
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.Counter        as C
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
          Right (result :: User.UserProfile) <- parseFile $ "data/" </> filename
          User._userCredsName (User._userProfileCreds result)
            `shouldBe` username
    mapM_
      go
      [ ("alice.json"  , "alice")
      , ("bob-0.json"  , "bob")
      , ("bob-1.json"  , "bob")
      , ("bob-2.json"  , "bob")
      , ("charlie.json", "charlie")
      ]

  describe "Command-line interface parser" $ do
    let go (arguments, command) =
          it ("Parses '" <> T.unpack arguments <> "'") $ do
            let A.Success x =
                  A.execParserPure A.defaultPrefs Command.parserInfo
                    $   T.unpack
                    <$> words arguments
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
                  $   T.unpack
                  <$> words arguments

          (Run.run $ Command.CommandWithTarget
              command
              (Command.StateFileTarget stateFile)
              (Command.User $ User.UserName "alice")
            )
            `shouldThrow` (== ExitSuccess)

          Right value <- parseFile stateFile
          value `shouldBe` state

    malice <- runIO $ parseFile "data/alice.json"
    case malice of
      Right alice -> do
        let aliceState = Data.emptyHask
              { Data._dbNextUserId   = C.CounterValue 2
              , Data._dbUserProfiles = Identity [alice]
              }
        mapM_
          go
          [ ("init" , Data.emptyHask)
          , ("user create alice a alice@example.com --accept-tos", aliceState)
          , ("reset", Data.emptyHask)
          ]


--------------------------------------------------------------------------------
parseFile path = do
  content <- readFile path
  pure $ Aeson.eitherDecodeStrict (T.encodeUtf8 content)
