{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.UserSpec
  ( spec
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

nonEmptyAlphaNum chars = not (null chars) && all C.isAlphaNum chars

instance Q.Arbitrary UserId where
  arbitrary = UserId . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

instance Q.Arbitrary UserPassword where
  arbitrary =
    UserPassword . Secret . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

userLoginParseProp :: UserId -> UserPassword -> Bool
userLoginParseProp userId userPass =
  let input = T.intercalate " " ["UserLogin", quote userId, quote userPass]
  in  tryParser (UserLogin userId userPass) dbSelectParser input

selectUserByIdProp :: UserId -> Bool
selectUserByIdProp userId =
  let input = T.intercalate " " ["SelectUserById", quote userId]
  in  tryParser (SelectUserById userId) dbSelectParser input

quote textWrapper = "'" <> textWrapper ^. coerced <> "'"

tryParser :: Eq a => a -> P.ParserText a -> Text -> Bool
tryParser expected parser input = Right expected == first
  (T.pack . P.errorBundlePretty)
  (P.parse parser (T.unpack input) input)
