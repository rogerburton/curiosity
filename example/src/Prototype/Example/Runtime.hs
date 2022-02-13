{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prototype.Example.Runtime
  ( Conf(..)
  , confRepl
  , confServer
  , ServerConf(..)
  , Runtime(..)
  , rConf
  , rDb
  , ExampleAppM(..)
  , boot
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Data.Todo   as Todo
import qualified Prototype.Example.Data.User   as User
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Types.Secret         ( (=:=) )

newtype ServerConf = ServerConf { _serverPort :: Int }
                   deriving Show
data Conf = Conf
  { _confRepl   :: Repl.ReplConf
  , _confServer :: ServerConf
  }
  deriving Show

makeLenses ''Conf

-- | The runtime, a central product type that should contain all our runtime supporting values. 
data Runtime = Runtime
  { _rConf :: Conf -- ^ The application configuration.
  , _rDb   :: Data.StmDb Runtime -- ^ The Storage. 
  }

makeLenses ''Runtime

instance Data.RuntimeHasStmDb Runtime where
  stmDbFromRuntime = _rDb

newtype ExampleAppM a = ExampleAppM { runExampleAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           )

-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage ExampleAppM User.UserProfile where
  dbUpdate = \case

    User.UserCreate newProfile -> onUserExists newProfileId createNew existsErr
     where
      newProfileId = S.dbId newProfile
      createNew =
        withUserStorage $ modifyUserProfiles newProfileId (newProfile :)
      existsErr = Errs.throwError' . User.UserExists . show

    User.UserDelete id -> onUserExists id (userNotFound id) deleteUser
     where
      deleteUser _ =
        withUserStorage $ modifyUserProfiles id (filter $ (/= id) . S.dbId)

    User.UserUpdate updatedProfile -> onUserExists id
                                                   (userNotFound id)
                                                   updateUser
     where
      id = S.dbId updatedProfile
      updateUser _ = withUserStorage $ modifyUserProfiles id replaceOlder
      replaceOlder users =
        [ if S.dbId u == id then updatedProfile else u | u <- users ]

   where
    modifyUserProfiles id f userProfiles =
      liftIO $ STM.atomically (STM.modifyTVar userProfiles f) $> [id]

  dbSelect = \case
    User.UserLogin id (User.UserPassword passInput) -> onUserExists
      id
      (userNotFound id)
      comparePass
     where
      comparePass foundUser@User.UserProfile { _userProfilePassword = User.UserPassword passStored }
        | passStored =:= passInput
        = pure [foundUser]
        | otherwise
        = Errs.throwError' . User.IncorrectPassword $ "Passwords don't match!"

    User.SelectUserById id ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== id) . S.dbId)

onUserExists id onNone onExisting =
  S.dbSelect (User.SelectUserById id) <&> headMay >>= maybe onNone onExisting
userNotFound = Errs.throwError' . User.UserNotFound . show
withUserStorage f = asks (Data._dbUserProfiles . _rDb) >>= f

instance S.DBStorage ExampleAppM Todo.TodoList where

  dbUpdate = \case
    Todo.AddItem id item -> onTodoListExists id
                                             (todoListNotFound id)
                                             modifyList

     where
      modifyList list' =
        let newList =
              list' { Todo._todoListItems = item : Todo._todoListItems list' }
        in  replaceTodoList newList $> [id]

    Todo.DeleteItem id itemName -> onTodoListExists id
                                                    (todoListNotFound id)
                                                    modifyList
     where
      modifyList list' =
        let newList = list'
              { Todo._todoListItems = filter
                                        ((/= itemName) . Todo._todoItemName)
                                        (Todo._todoListItems list')
              }
        in  replaceTodoList newList $> [id]

    Todo.MarkItem id itemName itemState -> onTodoListExists
      id
      (todoListNotFound id)
      modifyList
     where
      modifyList list' =
        let
          newList = list'
            { Todo._todoListItems = fmap replaceItem (Todo._todoListItems list')
            }
          replaceItem item@Todo.TodoListItem {..}
            | _todoItemName == itemName = item { Todo._todoItemState = itemState
                                               }
            | otherwise = item
        in
          replaceTodoList newList $> [id]

  dbSelect = \case
    Todo.SelectTodoListById id -> filtStoredTodos $ (== id) . S.dbId
    Todo.SelectTodoListsByPendingItems ->
      filtStoredTodos
        $ any ((== Todo.TodoListItemPending) . Todo._todoItemState)
        . Todo._todoListItems
    Todo.SelectTodoListsByUser userId ->
      filtStoredTodos $ elem userId . Todo._todoListUsers

withTodoStorage f = asks (Data._dbTodos . _rDb) >>= f
filtStoredTodos f =
  withTodoStorage $ liftIO . STM.readTVarIO >=> pure . filter f

onTodoListExists id onNone onExisting =
  S.dbSelect (Todo.SelectTodoListById id)
    <&> headMay
    >>= maybe onNone onExisting

todoListNotFound = Errs.throwError' . Todo.TodoListNotFound . show

replaceTodoList newList =
  let replaceList list' | S.dbId list' == S.dbId newList = newList
                        | otherwise                      = list'
  in  withTodoStorage $ \stmLists ->
        liftIO . STM.atomically $ STM.modifyTVar' stmLists $ fmap replaceList

-- | Boot up a runtime.
boot
  :: MonadIO m
  => Conf
  -> Maybe (Data.HaskDb Runtime)
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf mInitDb = do
  _rDb <- maybe Data.instantiateEmptyStmDb Data.instantiateStmDb mInitDb
  pure $ Right Runtime { .. }
