{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.UserSpec
  ( spec
  , showUserUpdate
  , showUserDelete
  , showUserCreate
  , showSelectUserById
  , showUserLogin
  ) where

import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Prototype.Example.Data.Shared as S
import           Prototype.Example.Data.User
import qualified Prototype.Example.Repl.Parse  as P
import           Prototype.Types.Secret
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

instance Q.Arbitrary UserPassword where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary UserName where
  arbitrary = S.nonEmptyAlphaNumGen

userLoginParseProp :: UserId -> UserPassword -> Bool
userLoginParseProp userId userPass =
  let input = showUserLogin userId userPass
  in  S.tryParser (UserLogin userId userPass) dbSelectParser input

showUserLogin userId userPass =
  T.intercalate " " ["UserLogin", S.quote userId, S.quote userPass]

selectUserByIdProp :: UserId -> Bool
selectUserByIdProp userId =
  let input = showSelectUserById userId
  in  S.tryParser (SelectUserById userId) dbSelectParser input

showSelectUserById userId =
  T.intercalate " " ["SelectUserById", S.quote userId]

userCreateParseProp :: UserId -> UserName -> UserPassword -> Bool
userCreateParseProp userId userName userPass =
  let input = showUserCreate userId userName userPass
  in  S.tryParser (UserCreate $ UserProfile userId userName userPass)
                  dbUpdateParser
                  input

showUserCreate userId userName userPass = T.intercalate
  " "
  ["UserCreate", S.quote userId, S.quote userName, S.quote userPass]

userUpdateParseProp :: UserId -> UserName -> UserPassword -> Bool
userUpdateParseProp userId userName userPass =
  let input = showUserUpdate userId userName userPass
  in  S.tryParser (UserUpdate $ UserProfile userId userName userPass)
                  dbUpdateParser
                  input

showUserUpdate userId userName userPass = T.intercalate
  " "
  ["UserUpdate", S.quote userId, S.quote userName, S.quote userPass]

userDeleteParseProp :: UserId -> Bool
userDeleteParseProp userId =
  let input = showUserDelete userId
  in  S.tryParser (UserDelete userId) dbUpdateParser input

showUserDelete userId = T.intercalate " " ["UserDelete", S.quote userId]

