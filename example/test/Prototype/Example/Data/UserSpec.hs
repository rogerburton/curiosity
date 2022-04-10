{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.UserSpec
  ( spec
  , showUserUpdate
  , showUserDelete
  , showUserCreate
  , showSelectUserById
  , showUserLogin
  ) where

import qualified Data.Text                     as T
import qualified Prototype.Example.Data.Shared as S
import           Prototype.Example.Data.User
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
    it "should parse UserUpdate inputs." $ Q.property userUpdateParseProp
    it "should parse UserDelete inputs." $ Q.property userDeleteParseProp

instance Q.Arbitrary UserId where
  arbitrary = S.nonEmptyAlphaNumGen

-- instance Q.Arbitrary (NonEmpty UserId) where
--   arbitrary = Q.arbitrary `Q.suchThatMap` nonEmpty

instance Q.Arbitrary Password where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary UserName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary UserCreds where
  arbitrary = UserCreds <$> Q.arbitrary <*> Q.arbitrary

userLoginParseProp :: UserCreds -> Bool
userLoginParseProp creds =
  let input = showUserLogin creds
  in  S.tryParser (UserLogin creds) dbSelectParser input

showUserLogin creds = T.intercalate " " ["UserLogin", showUserCreds creds]

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

userUpdateParseProp :: UserCreds -> UserName -> Bool
userUpdateParseProp creds@(UserCreds userId userPass) userName =
  let input = showUserUpdate userId userName userPass
  in  S.tryParser (UserUpdate $ UserProfile creds userName) dbUpdateParser input

showUserUpdate userId userName userPass = T.intercalate
  " "
  ["UserUpdate", S.quote userId, S.quote userPass, S.quote userName]

userDeleteParseProp :: UserId -> Bool
userDeleteParseProp userId =
  let input = showUserDelete userId
  in  S.tryParser (UserDelete userId) dbUpdateParser input

showUserDelete userId = T.intercalate " " ["UserDelete", S.quote userId]

