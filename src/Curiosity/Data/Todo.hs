{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module: Curiosity.Data.Todo
Description: All datatypes related to Todos
-}
module Curiosity.Data.Todo
  ( TodoList(..)
  , TodoListItem(..)
  , TodoListItemState(..)
  -- * Newtypes to aid with type-safety
  , TodoListName(..)
  , TodoListItemName(..)
  , TodoListItemDesc(..)
  -- * Export all DB operations
  , Storage.DBUpdate(..)
  , Storage.DBSelect(..)
  -- * Lenses 
  , todoListName
  , todoListItems
  , todoListUsers
  -- * Errors
  , TodoListErr(..)
  ) where

import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as Storage
import           Control.Lens
import qualified Curiosity.Data.User           as U
import           Data.Aeson
import           Data.Default.Class
import qualified Network.HTTP.Types            as HTTP

-- | The name of a `TodoList`.
newtype TodoListName = TodoListName Text
                     deriving (Eq, Show, IsString, ToJSON, FromJSON) via Text

-- | A simple todo-list
data TodoList = TodoList
  { _todoListName  :: TodoListName -- ^ Name of the TodoList; currently we treat this also as the ID, for simplicity.
                                   -- This means for the example use case, the names must be unique. 
  , _todoListItems :: [TodoListItem] -- ^ Current list of items. 
  , _todoListUsers :: [U.UserId] -- ^ Users this todo-list is accessible to.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TodoListItemName = TodoListItemName Text
                     deriving (Eq, Show, IsString, ToJSON, FromJSON) via Text

newtype TodoListItemDesc = TodoListItemDesc Text
                     deriving (Eq, Show, IsString, ToJSON, FromJSON) via Text

data TodoListItem = TodoListItem
  { _todoItemName  :: TodoListItemName -- ^ Name of an item, mandatory. 
  , _todoItemDesc  :: Maybe TodoListItemDesc -- ^ Item description, optional. 
  , _todoItemState :: TodoListItemState -- ^ Item state. 
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TodoListItemState = TodoListItemPending
                       | TodoListItemComplete
                       deriving (Eq, Show, Enum, Bounded, Ord, Generic)
                       deriving anyclass (ToJSON, FromJSON)

instance Default TodoListItemState where
  def = TodoListItemPending

-- | The `Storage.DBIdentity` instance uses the `TodoListName` of a `TodoList` product type as the unique "DB" ID. 
instance Storage.DBIdentity TodoList where
  type DBId TodoList = TodoListName
  dbId = _todoListName

instance Storage.DBStorageOps TodoList where
  
  data DBSelect TodoList =
    -- | Select a list by its ID.
    SelectTodoListById (Storage.DBId TodoList)
    -- | Select the lists with at least 1 pending item in them. 
    | SelectTodoListsByPendingItems
    -- | Select the lists that are accessible to a given `U.UserId`.
    | SelectTodoListsByUser U.UserId
    deriving (Eq, Show)
  
  data DBUpdate TodoList =
    -- | Create a new todo-list
    CreateList TodoList
    -- | Add an item to an existing todo-list
    | AddItem (Storage.DBId TodoList) TodoListItem
    -- | Delete an item from a todo-list
    | DeleteItem (Storage.DBId TodoList) TodoListItemName
    -- | Mark an item in a todo-list
    | MarkItem (Storage.DBId TodoList) TodoListItemName TodoListItemState
    -- | Add a user to a todo-list (the todo-list becomes accessible to this user)
    | AddUsersToList (Storage.DBId TodoList) (NonEmpty U.UserId)
    -- | Delete a todo-list
    | DeleteList (Storage.DBId TodoList)
    -- | Remove users from a list
    | RemoveUsersFromList (Storage.DBId TodoList) (NonEmpty U.UserId)
    deriving (Eq, Show)

data TodoListErr = TodoListNotFound Text
                    | TodoListExists TodoListName
                    deriving Show

instance Errs.IsRuntimeErr TodoListErr where
  errCode = errCode' . \case
    TodoListNotFound{} -> "NOT_FOUND"
    TodoListExists{}   -> "EXISTS"
    where errCode' = mappend "ERR.TODO_LIST."

  userMessage = Just . \case
    TodoListNotFound msg -> msg
    TodoListExists   id  -> show id

  httpStatus = \case
    TodoListNotFound{} -> HTTP.notFound404
    TodoListExists{}   -> HTTP.conflict409

makeLenses ''TodoList

