{-# LANGUAGE TypeFamilies #-}
{- |
Module: Prototype.Example.Data.Todo
Description: All datatypes related to Todos
-}
module Prototype.Example.Data.Todo
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
  -- ** Parsers
  , dbUpdateParser
  , dbSelectParser
  ) where

import           Data.Default.Class
import qualified Prototype.Example.Data.User   as U
import qualified Prototype.Example.Repl.Parse  as P
import qualified Prototype.Runtime.Storage     as Storage

-- | The name of a `TodoList`.
newtype TodoListName = TodoListName Text
                     deriving (Eq, Show, IsString) via Text

-- | A simple todo-list
data TodoList = TodoList
  { _todoListName  :: TodoListName -- ^ Name of the TodoList; currently we treat this also as the ID, for simplicity.
                                   -- This means for the example use case, the names must be unique. 
  , _todoListItems :: [TodoListItem] -- ^ Current list of items. 
  , _todoListUsers :: [U.UserId] -- ^ Users this todo-list is accessible to.
  }
  deriving Show

newtype TodoListItemName = TodoListItemName Text
                     deriving (Eq, Show, IsString) via Text

newtype TodoListItemDesc = TodoListItemDesc Text
                     deriving (Eq, Show, IsString) via Text


data TodoListItem = TodoListItem
  { _todoItemName  :: TodoListItemName -- ^ Name of an item, mandatory. 
  , _todoItemDesc  :: Maybe TodoListItemDesc -- ^ Item description, optional. 
  , _todoItemState :: TodoListItemState -- ^ Item state. 
  }
  deriving Show

data TodoListItemState = TodoListItemPending
                   | TodoListItemComplete
                   deriving Show

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
    deriving Show
  
  data DBUpdate TodoList =
    AddItem (Storage.DBId TodoList) TodoListItem
    | DeleteItem (Storage.DBId TodoList) TodoListItemName
    | MarkItem (Storage.DBId TodoList) TodoListItemName TodoListItemState
    deriving Show

dbUpdateParser :: P.ParserText (Storage.DBUpdate TodoList)
dbUpdateParser = undefined

dbSelectParser :: P.ParserText (Storage.DBSelect TodoList)
dbSelectParser = undefined
