{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.UserSpec
  ( spec
  , showUserUpdate
  , showUserDelete
  , showUserCreate
  , showSelectUserById
  , showUserLogin
  ) where

import           Control.Lens
import qualified Data.Char                     as C
import qualified Data.Text                     as T
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

nonEmptyAlphaNum chars = not (null chars) && all C.isAlphaNum chars

instance Q.Arbitrary UserId where
  arbitrary = UserId . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

instance Q.Arbitrary UserPassword where
  arbitrary =
    UserPassword . Secret . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

instance Q.Arbitrary UserName where
  arbitrary = UserName . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

userLoginParseProp :: UserId -> UserPassword -> Bool
userLoginParseProp userId userPass =
  let input = showUserLogin userId userPass
  in  tryParser (UserLogin userId userPass) dbSelectParser input

showUserLogin userId userPass =
  T.intercalate " " ["UserLogin", quote userId, quote userPass]

selectUserByIdProp :: UserId -> Bool
selectUserByIdProp userId =
  let input = showSelectUserById userId
  in  tryParser (SelectUserById userId) dbSelectParser input

showSelectUserById userId = T.intercalate " " ["SelectUserById", quote userId]

userCreateParseProp :: UserId -> UserName -> UserPassword -> Bool
userCreateParseProp userId userName userPass =
  let input = showUserCreate userId userName userPass
  in  tryParser (UserCreate $ UserProfile userId userName userPass)
                dbUpdateParser
                input

showUserCreate userId userName userPass =
  T.intercalate " " ["UserCreate", quote userId, quote userName, quote userPass]

userUpdateParseProp :: UserId -> UserName -> UserPassword -> Bool
userUpdateParseProp userId userName userPass =
  let input = showUserUpdate userId userName userPass
  in  tryParser (UserUpdate $ UserProfile userId userName userPass)
                dbUpdateParser
                input

showUserUpdate userId userName userPass =
  T.intercalate " " ["UserUpdate", quote userId, quote userName, quote userPass]

userDeleteParseProp :: UserId -> Bool
userDeleteParseProp userId =
  let input = showUserDelete userId
  in  tryParser (UserDelete userId) dbUpdateParser input

showUserDelete userId = T.intercalate " " ["UserDelete", quote userId]

quote textWrapper = "'" <> textWrapper ^. coerced <> "'"

tryParser :: Eq a => a -> P.ParserText a -> Text -> Bool
tryParser expected parser input = Right expected == first
  (T.pack . P.errorBundlePretty)
  (P.parse parser (T.unpack input) input)

