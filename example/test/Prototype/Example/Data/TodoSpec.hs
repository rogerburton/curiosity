{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.TodoSpec
  ( spec
  ) where

import qualified Prototype.Example.Data.Shared as S
import           Prototype.Example.Data.Todo
import           Test.Hspec
import qualified Test.QuickCheck               as Q

spec :: Spec
spec = describe "Parsing todo-visualisations" $ do
  it "should parse SelectTodoListById inputs."
    $ Q.property selectTodoListByIdProp
  it "should parse SelectTodoListsByPendingItems inputs."
    $ Q.property selectTodoListsByPendingItemsProp

instance Q.Arbitrary TodoListName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary TodoListItemDesc where
  arbitrary = S.nonEmptyAlphaNumGen

selectTodoListByIdProp :: TodoListName -> Bool
selectTodoListByIdProp name =
  let input = "SelectTodoListById " <> S.quote name
  in  S.tryParser (SelectTodoListById name) dbSelectParser input

selectTodoListsByPendingItemsProp :: TodoListName -> Bool
selectTodoListsByPendingItemsProp name =
  let input = "SelectTodoListsByPendingItems "
  in  S.tryParser SelectTodoListsByPendingItems dbSelectParser input

-- instance Q.Arbitrary UserPassword where
--   arbitrary =
--     UserPassword . Secret . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

-- instance Q.Arbitrary UserName where
--   arbitrary = UserName . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

