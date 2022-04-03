module Prototype.Example.DataSpec
  ( spec
  ) where

import           Prototype.Example.Data
import           Prototype.Example.Data.User
import qualified Prototype.Example.Data.UserSpec
                                               as US
import           Prototype.Types.Secret
import           Test.Hspec
import qualified Test.QuickCheck               as Q

spec :: Spec
spec = do
  describe "Parsing visualisations" $ do
    -- FIXME add tests for TODO
    it "Should parse UserLogin-visualisations." $ Q.property userVizParseProp
    it "Should parse SelectUserById-visualisation."
      $ Q.property selectUserByIdVizParseProp

  describe "Parsing modifications" $ do
    -- FIXME add tests for TODO
    it "Should parse UserCreate-modifications."
      $ Q.property createUserModParseProp
    it "Should parse UserDelete-modifications."
      $ Q.property deleteUserModParseProp

userVizParseProp :: UserId -> Password -> Bool
userVizParseProp userId userPass =
  let input = "viz user " <> US.showUserLogin userId userPass
  in  isRight $ parseViz input

selectUserByIdVizParseProp :: UserId -> Bool
selectUserByIdVizParseProp userId =
  let input = "viz user " <> US.showSelectUserById userId
  in  isRight $ parseViz input

createUserModParseProp :: UserId -> UserName -> Password -> Bool
createUserModParseProp userId userName userPass =
  let input = "mod user " <> US.showUserCreate userId userName userPass
  in  isRight $ parseMod input

deleteUserModParseProp :: UserId -> Bool
deleteUserModParseProp userId =
  let input = "mod user " <> US.showUserDelete userId
  in  isRight $ parseMod input
