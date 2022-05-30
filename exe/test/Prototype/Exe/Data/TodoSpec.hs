{-# OPTIONS_GHC -Wno-orphans  #-}
module Prototype.Exe.Data.TodoSpec
  ( spec
  ) where

import qualified Data.Text                     as T
import qualified Prototype.Exe.Data.Shared     as S
import           Prototype.Exe.Data.Todo
import qualified Prototype.Exe.Data.User       as U
import           Prototype.Exe.Data.UserSpec    ( ) -- Q.Arbitrary instance for U.UserId
import qualified Prototype.Runtime.Storage     as Storage
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
    it "should parse DeleteListinputs." $ Q.property deleteListProp
    it "should parse AddUsersToList." $ Q.property addUsersToListProp
    it "should parse RemoveUsersFromList." $ Q.property removeUsersFromListProp

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

deleteListProp :: TodoListName -> Bool
deleteListProp listName =
  let input = T.unwords ["DeleteList", S.quote listName]
  in  S.tryParser (DeleteList listName) dbUpdateParser input

addUsersToListProp :: TodoListName -> NonEmpty U.UserId -> Bool
addUsersToListProp = userOnListProp "AddUsersToList" AddUsersToList

removeUsersFromListProp :: TodoListName -> NonEmpty U.UserId -> Bool
removeUsersFromListProp =
  userOnListProp "RemoveUsersFromList" RemoveUsersFromList

userOnListProp
  :: Text
  -> (TodoListName -> NonEmpty U.UserId -> Storage.DBUpdate TodoList)
  -> TodoListName
  -> NonEmpty U.UserId
  -> Bool
userOnListProp constructorName constructor listName users =
  let input = T.unwords
        [constructorName, S.quote listName, S.showListWith S.quote users]
  in  S.tryParser (constructor listName users) dbUpdateParser input
