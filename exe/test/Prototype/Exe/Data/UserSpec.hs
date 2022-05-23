{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Exe.Data.UserSpec
  ( spec
  , showUserPasswordUpdate
  , showUserDelete
  , showUserCreate
  , showSelectUserById
  , showUserLogin
  ) where

import qualified Data.Text                     as T
import qualified Prototype.Exe.Data.Shared     as S
import           Prototype.Exe.Data.User
import           Prototype.Types.Secret  hiding ( Password )
import           Test.Hspec
import qualified Test.QuickCheck               as Q

spec :: Spec
spec = do
  describe "Parsing user-visualisations" $ do
    it "should parse UserLogin inputs." $ Q.property userLoginParseProp
    it "should parse SelectUserById inputs." $ Q.property selectUserByIdProp
  describe "Parsing user-mods" $ do
    it "should parse UserCreate inputs." $ Q.property userCreateParseProp
    it "should parse UserPasswordUpdate inputs."
      $ Q.property userUpdateParseProp
    it "should parse UserDelete inputs." $ Q.property userDeleteParseProp

instance Q.Arbitrary UserId where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary Password where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary UserName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary UserCreds where
  arbitrary = UserCreds <$> Q.arbitrary <*> Q.arbitrary

userLoginParseProp :: UserName -> Password -> Bool
userLoginParseProp userName pwd =
  let input = showUserLogin userName pwd
  in  S.tryParser (UserLoginWithUserName userName pwd) dbSelectParser input

showUserLogin :: UserName -> Password -> Text
showUserLogin userName pwd =
  T.intercalate " " ["UserLoginWithUserName", S.quote userName, S.quote pwd]

selectUserByIdProp :: UserId -> Bool
selectUserByIdProp userId =
  let input = showSelectUserById userId
  in  S.tryParser (SelectUserById userId) dbSelectParser input

showSelectUserById userId =
  T.intercalate " " ["SelectUserById", S.quote userId]

userCreateParseProp :: UserCreds -> UserName -> Bool
userCreateParseProp creds userName =
  let input = showUserCreate creds userName
  in  S.tryParser (UserCreate $ UserProfile creds userName) dbUpdateParser input

showUserCreate creds userName =
  T.intercalate " " ["UserCreate", showUserCreds creds, S.quote userName]

showUserCreds (UserCreds id pass') =
  T.intercalate " " [S.quote id, S.quote pass']

userUpdateParseProp :: UserId -> Password -> Bool
userUpdateParseProp userId pwd =
  let input = showUserPasswordUpdate userId pwd
  in  S.tryParser (UserPasswordUpdate userId pwd) dbUpdateParser input

showUserPasswordUpdate userId userPass =
  T.intercalate " " ["UserPasswordUpdate", S.quote userId, S.quote userPass]

userDeleteParseProp :: UserId -> Bool
userDeleteParseProp userId =
  let input = showUserDelete userId
  in  S.tryParser (UserDelete userId) dbUpdateParser input

showUserDelete userId = T.intercalate " " ["UserDelete", S.quote userId]

