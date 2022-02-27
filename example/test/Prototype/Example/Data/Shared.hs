module Prototype.Example.Data.Shared
  ( quote
  , nonEmptyAlphaNum
  -- * Commonly needed generators.
  , nonEmptyAlphaNumGen
  -- * Parser based properties 
  , tryParser
  ) where

import           Control.Lens
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Prototype.Example.Repl.Parse  as P
import qualified Test.QuickCheck               as Q

quote :: forall a s . (Semigroup a, IsString a, Coercible s a) => s -> a
quote textWrapper = "'" <> textWrapper ^. coerced <> "'"

nonEmptyAlphaNum chars = not (null chars) && all C.isAlphaNum chars

{- | Generator based off a coercible of Text; ideal for derivation of newtypes. 
Consider:

@
  instance Q.Arbitrary TodoListItemDesc where
  arbitrary = S.nonEmptyAlphaNumGen
@

-}
nonEmptyAlphaNumGen :: Coercible Text a => Q.Gen a
nonEmptyAlphaNumGen =
  view coerced . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

tryParser :: Eq a => a -> P.ParserText a -> Text -> Bool
tryParser expected parser input = Right expected == first
  (T.pack . P.errorBundlePretty)
  (P.parse parser (T.unpack input) input)

