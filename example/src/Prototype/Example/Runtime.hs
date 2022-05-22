{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prototype.Example.Runtime
  ( Conf(..)
  , confRepl
  , confServer
  , confLogging
  , confCookie
  , confMkJwtSettings
  , ServerConf(..)
  , Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , rJwtSettings
  , ExampleAppM(..)
  , boot
  , runExampleAppMSafe
  -- * Servant compat
  , exampleAppMHandlerNatTrans
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Crypto.JOSE.JWK               as JWK
import qualified Data.List                     as L
import qualified MultiLogging                  as ML
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Data.Todo   as Todo
import qualified Prototype.Example.Data.User   as User
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Types.Secret         ( (=:=) )
import qualified Servant
import qualified Servant.Auth.Server           as Srv

newtype ServerConf = ServerConf { _serverPort :: Int }
                   deriving Show

-- | Application config.
data Conf = Conf
  { _confRepl          :: Repl.ReplConf -- ^ Config. for the REPL.
  , _confServer        :: ServerConf -- ^ Config. for the HTTP server.
  , _confLogging       :: ML.LoggingConf -- ^ Logging configuration.
  , _confCookie        :: Srv.CookieSettings -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _confMkJwtSettings :: JWK.JWK -> Srv.JWTSettings -- ^ JWK settings to use, depending on the key employed.
  }

makeLenses ''Conf

-- | The runtime, a central product type that should contain all our runtime supporting values. 
data Runtime = Runtime
  { _rConf        :: Conf -- ^ The application configuration.
  , _rDb          :: Data.StmDb Runtime -- ^ The Storage. 
  , _rLoggers     :: ML.AppNameLoggers -- ^ Multiple loggers to log over. 
  , _rJwtSettings :: Srv.JWTSettings -- ^ JWT settings to use.
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
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

-- | Run the `ExampleAppM` computation catching all possible exceptions. 
runExampleAppMSafe
  :: forall a m
   . MonadIO m
  => Runtime
  -> ExampleAppM a
  -> m (Either Errs.RuntimeErr a)
runExampleAppMSafe rt (ExampleAppM op') =
  liftIO
    . fmap (join . first Errs.RuntimeException)
    . try @SomeException
    . runExceptT
    $ runReaderT op' rt

-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage ExampleAppM User.UserProfile where
  dbUpdate = \case

    User.UserCreate newProfile -> onUserIdExists newProfileId
                                                 createNew
                                                 existsErr
     where
      newProfileId = S.dbId newProfile
      createNew    = onUserNameExists
        (newProfile ^. User.userProfileName)
        (withUserStorage $ modifyUserProfiles newProfileId (newProfile :))
        existsErr
      existsErr = Errs.throwError' . User.UserExists . show

    User.UserCreateGeneratingUserId userName password -> do
      -- generate a new and random user-id
      newId <- User.genRandomUserId 10
      let newProfile =
            User.UserProfile (User.UserCreds newId password) userName
      S.dbUpdate $ User.UserCreate newProfile

    User.UserDelete id -> onUserIdExists id (userNotFound $ show id) deleteUser
     where
      deleteUser _ =
        withUserStorage $ modifyUserProfiles id (filter $ (/= id) . S.dbId)

    User.UserPasswordUpdate id newPass -> onUserIdExists
      id
      (userNotFound $ show id)
      updateUser
     where
      updateUser _ = withUserStorage $ modifyUserProfiles id replaceOlder
      setPassword = set (User.userCreds . User.userCredsPassword) newPass
      replaceOlder users =
        [ if S.dbId u == id then setPassword u else u | u <- users ]

   where
    modifyUserProfiles id f userProfiles =
      liftIO $ STM.atomically (STM.modifyTVar userProfiles f) $> [id]

  dbSelect = \case

    User.UserLoginWithUserName userName (User.Password passInput) ->
      -- Try to look up an unambigous user-id using the human friendly name, and then execute UserLogin.
      S.dbSelect (User.SelectUserByUserName userName) >>= \case
        [u] | passwordsMatch -> pure [u]
         where
          passwordsMatch = storedPass =:= passInput
          User.Password storedPass =
            u ^. User.userCreds . User.userCredsPassword
        _ -> userNotFound $ "No user with userName = " <> userName ^. coerced

    User.SelectUserById id ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== id) . S.dbId)

    User.SelectUserByUserName userName ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== userName) . User._userProfileName)

-- | Support for logging for the example application 
instance ML.MonadAppNameLogMulti ExampleAppM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)

onUserIdExists id onNone onExisting =
  S.dbSelect (User.SelectUserById id) <&> headMay >>= maybe onNone onExisting
onUserNameExists userName onNone onExisting =
  S.dbSelect (User.SelectUserByUserName userName)
    <&> headMay
    >>= maybe onNone onExisting
userNotFound =
  Errs.throwError' . User.UserNotFound . mappend "User not found: "
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

    Todo.DeleteList id -> withTodoStorage $ \todoStm -> do
      todos <- liftIO . STM.readTVarIO $ todoStm
      let existing = find ((== id) . S.dbId) todos
      if isNothing existing
        then todoListNotFound id
        else
          liftIO
              ( STM.atomically
              $ STM.modifyTVar' todoStm (filter $ (/= id) . S.dbId)
              )
            $> [id]
    Todo.AddUsersToList id users -> onTodoListExists id
                                                     (todoListNotFound id)
                                                     modifyList
     where
      modifyList list' =
        let newList =
              list' & Todo.todoListUsers %~ L.nub . mappend (toList users)
        in  replaceTodoList newList $> [id]
    Todo.RemoveUsersFromList id users -> onTodoListExists
      id
      (todoListNotFound id)
      modifyList
     where
      modifyList list' =
        let newList = list' & Todo.todoListUsers %~ (L.\\ (toList users))
        in  replaceTodoList newList $> [id]

    Todo.CreateList newList -> withTodoStorage $ \todoStm -> do
      todos <- liftIO . STM.readTVarIO $ todoStm
      let existing = find ((== newId) . S.dbId) todos
          newId    = S.dbId newList
      if isJust existing
        then existsErr newId
        else
          liftIO (STM.atomically $ STM.modifyTVar' todoStm (newList :))
            $> [newId]
      where existsErr = Errs.throwError' . Todo.TodoListExists

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
  => Conf -- ^ configuration to boot with.
  -> Maybe (Data.HaskDb Runtime) -- ^ Initial state, if any.
  -> JWK.JWK -- ^ A Key for JSON Web Tokens (used to encrypt auth. cookies). 
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf mInitDb jwk = do
  _rDb      <- maybe Data.instantiateEmptyStmDb Data.instantiateStmDb mInitDb
  _rLoggers <- ML.makeDefaultLoggersWithConf $ _rConf ^. confLogging

  pure $ Right Runtime { _rJwtSettings = (_rConf ^. confMkJwtSettings) jwk, .. }

-- | Natural transformation from some `ExampleAppM` in any given mode, to a servant Handler. 
exampleAppMHandlerNatTrans
  :: forall a . Runtime -> ExampleAppM a -> Servant.Handler a
exampleAppMHandlerNatTrans rt appM =
  let
    -- We peel off the ExampleAppM + ReaderT layers, exposing our ExceptT RuntimeErr IO a
    -- This is very similar to Servant's Handler: https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runExampleAppM $ appM
      -- Map our errors to `ServantError` 
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in 
    -- re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT

