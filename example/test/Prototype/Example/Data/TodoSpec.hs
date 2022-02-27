{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Example.Data.TodoSpec
  ( spec
  ) where

import qualified Data.Text                     as T
import qualified Prototype.Example.Data.Shared as S
import           Prototype.Example.Data.Todo
import qualified Prototype.Example.Data.User   as U
import           Prototype.Example.Data.UserSpec
                                                ( ) -- Q.Arbitrary instance for U.UserId
import           Test.Hspec
import qualified Test.QuickCheck               as Q

spec :: Spec
spec = do
  describe "Parsing todo-visualisations" $ do
    it "should parse SelectTodoListById inputs."
      $ Q.property selectTodoListByIdProp
    it "should parse SelectTodoListsByPendingItems inputs."
      $ Q.property selectTodoListsByPendingItemsProp
    it "should parse SelectTodoListsByUser inputs."
      $ Q.property selectTodoListsByUserProp

  describe "Parsing todo-modificications" $ do
    it "should parse AddItem inputs." $ Q.property addItemProp
    it "should parse DeleteItem inputs." $ Q.property deleteItemProp
    it "should parse MarkItem inputs." $ Q.property markItemProp

instance Q.Arbitrary TodoListName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary TodoListItemDesc where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary TodoListItemName where
  arbitrary = S.nonEmptyAlphaNumGen

instance Q.Arbitrary TodoListItemState where
  arbitrary = Q.elements $ enumFromTo minBound maxBound

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

addItemProp
  :: TodoListName
  -> TodoListItemName
  -> Maybe TodoListItemDesc
  -> TodoListItemState
  -> Bool
addItemProp listName itemName mDesc status =
  let input = T.unwords
        [ "AddItem"
        , S.quote listName
        , S.quote itemName
        , maybe "''" S.quote mDesc
        , S.quote statusT
        ]
      statusT :: Text = showStatus status
  in  S.tryParser (AddItem listName $ TodoListItem itemName mDesc status)
                  dbUpdateParser
                  input
deleteItemProp :: TodoListName -> TodoListItemName -> Bool
deleteItemProp listName itemName =
  let input = T.unwords ["DeleteItem", S.quote listName, S.quote itemName]
  in  S.tryParser (DeleteItem listName itemName) dbUpdateParser input

markItemProp :: TodoListName -> TodoListItemName -> TodoListItemState -> Bool
markItemProp listName itemName itemState =
  let input = T.unwords
        [ "MarkItem"
        , S.quote listName
        , S.quote itemName
        , S.quote (showStatus itemState)
        ]
  in  S.tryParser (MarkItem listName itemName itemState) dbUpdateParser input

showStatus :: TodoListItemState -> Text
showStatus = \case
  TodoListItemComplete -> "complete"
  TodoListItemPending  -> "pending"

