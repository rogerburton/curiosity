{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
  , todoListNameParser
  , todoListItemNameParser
  , todoListItemStateParser
  , todoListItemDescParserMaybe
  , todoListItemParser
  -- * Lenses 
  , todoListName
  , todoListItems
  , todoListUsers
  -- * Errors
  , TodoListErr(..)
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Example.Data.User   as U
import qualified Prototype.Example.Repl.Parse  as P
import qualified Prototype.Runtime.Errors      as Errs
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
  deriving (Eq, Show)

newtype TodoListItemName = TodoListItemName Text
                     deriving (Eq, Show, IsString) via Text

newtype TodoListItemDesc = TodoListItemDesc Text
                     deriving (Eq, Show, IsString) via Text


data TodoListItem = TodoListItem
  { _todoItemName  :: TodoListItemName -- ^ Name of an item, mandatory. 
  , _todoItemDesc  :: Maybe TodoListItemDesc -- ^ Item description, optional. 
  , _todoItemState :: TodoListItemState -- ^ Item state. 
  }
  deriving (Eq, Show)

data TodoListItemState = TodoListItemPending
                       | TodoListItemComplete
                       deriving (Eq, Show, Enum, Bounded, Ord)

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

dbUpdateParser :: P.ParserText (Storage.DBUpdate TodoList)
dbUpdateParser = P.tryAlts
  [createList, addItem, deleteItem, markItem, addUsers, deleteList, removeUsers]
 where
  createList =
    P.withTrailSpaces "CreateList" *> (CreateList <$> todoListParser)
  addItem =
    P.withTrailSpaces "AddItem"
      *> (AddItem <$> todoListNameParser <* P.space <*> todoListItemParser)
  deleteItem =
    P.withTrailSpaces "DeleteItem"
      *> (   DeleteItem
         <$> todoListNameParser
         <*  P.space
         <*> todoListItemNameParser
         )
  markItem =
    P.withTrailSpaces "MarkItem"
      *> (   MarkItem
         <$> todoListNameParser
         <*  P.space
         <*> todoListItemNameParser
         <*  P.space
         <*> todoListItemStateParser
         )
  addUsers = uncurry AddUsersToList <$> listNameAndUserIds "AddUsersToList"
  removeUsers =
    uncurry RemoveUsersFromList <$> listNameAndUserIds "RemoveUsersFromList"
  deleteList =
    P.withTrailSpaces "DeleteList" *> (DeleteList <$> todoListNameParser)

  listNameAndUserIds name = P.withTrailSpaces name *> do
    listName <- todoListNameParser
    P.space
    mUserIds <- nonEmpty <$> P.parseListOf U.userIdParser
    maybe noUsers (pure . (listName, )) mUserIds
   where
    noUsers = P.fancyFailure . Set.singleton . P.ErrorFail $ "No user-ids!"

dbSelectParser :: P.ParserText (Storage.DBSelect TodoList)
dbSelectParser = selectById <|> selectByPendingItems <|> selectTodoListsByUser
 where
  selectById =
    P.withTrailSpaces "SelectTodoListById"
      *> (SelectTodoListById <$> todoListNameParser)
  selectByPendingItems =
    P.string' "SelectTodoListsByPendingItems" $> SelectTodoListsByPendingItems
  selectTodoListsByUser =
    P.withTrailSpaces "SelectTodoListsByUser"
      *> (SelectTodoListsByUser <$> U.userIdParser)

todoListParser :: P.ParserText TodoList
todoListParser =
  TodoList
    <$> todoListNameParser
    <*  P.space
    <*> P.parseListOf todoListItemParser
    <*  P.space
    <*> P.parseListOf U.userIdParser

todoListNameParser = TodoListName <$> P.punctuated P.alphaNumText
todoListItemNameParser = TodoListItemName <$> P.punctuated P.alphaNumText

todoListItemStateParser :: P.ParserText TodoListItemState
todoListItemStateParser = P.try pending <|> complete
 where
  pending  = P.punctuated (P.string' "pending") $> TodoListItemPending
  complete = P.punctuated (P.string' "complete") $> TodoListItemComplete

todoListItemDescParserMaybe :: P.ParserText (Maybe TodoListItemDesc)
todoListItemDescParserMaybe = do
  -- Read text; stripping whitespaces within the punctuated part. 
  txt <- T.strip <$> P.punctuated P.alphaNumText
  -- If the supplied text is not empty; we return a Just otherwise, we ignore the provided /blank/ description.
  pure $ if T.null txt then Nothing else Just (TodoListItemDesc txt)

todoListItemParser :: P.ParserText TodoListItem
todoListItemParser =
  TodoListItem
    <$> todoListItemNameParser
    <*  P.space
    <*> todoListItemDescParserMaybe
    <*  P.space
    <*> todoListItemStateParser

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

