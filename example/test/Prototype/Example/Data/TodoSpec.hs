{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.TodoSpec
  ( spec
  ) where

import qualified Prototype.Example.Data.Shared as S
import           Prototype.Example.Data.Todo
import qualified Prototype.Example.Data.User   as U
import           Prototype.Example.Data.UserSpec
                                                ( ) -- Q.Arbitrary instance for U.UserId
import           Test.Hspec
import qualified Test.QuickCheck               as Q

spec :: Spec
spec = describe "Parsing todo-visualisations" $ do
  it "should parse SelectTodoListById inputs."
    $ Q.property selectTodoListByIdProp
  it "should parse SelectTodoListsByPendingItems inputs."
    $ Q.property selectTodoListsByPendingItemsProp
  it "should parse SelectTodoListsByUser inputs."
    $ Q.property selectTodoListsByUserProp

instance Q.Arbitrary TodoListName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary TodoListItemDesc where
  arbitrary = S.nonEmptyAlphaNumGen

selectTodoListByIdProp :: TodoListName -> Bool
selectTodoListByIdProp name =
  let input = "SelectTodoListById " <> S.quote name
  in  S.tryParser (SelectTodoListById name) dbSelectParser input

selectTodoListsByPendingItemsProp :: Bool
selectTodoListsByPendingItemsProp =
  let input = "SelectTodoListsByPendingItems"
  in  S.tryParser SelectTodoListsByPendingItems dbSelectParser input

selectTodoListsByUserProp :: U.UserId -> Bool
selectTodoListsByUserProp userId =
  let input = "SelectTodoListsByUser " <> S.quote userId
  in  S.tryParser (SelectTodoListsByUser userId) dbSelectParser input

-- instance Q.Arbitrary UserPassword where
--   arbitrary =
--     UserPassword . Secret . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

-- instance Q.Arbitrary UserName where
--   arbitrary = UserName . T.pack <$> Q.arbitrary `Q.suchThat` nonEmptyAlphaNum

