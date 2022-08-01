{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Curiosity.Runtime
  ( IOErr(..)
  , Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , AppM(..)
  , boot
  , boot'
  , handleCommand
  , powerdown
  , readDb
  , readDbSafe
  , saveDb
  , saveDbAs
  , runAppMSafe
  , withRuntimeAtomically
  -- * High-level operations
  , createUser
  , checkCredentials
  -- * Servant compat
  , appMHandlerNatTrans
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Commence.Types.Secret          ( (=:=) )
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.Todo           as Todo
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as Command
import qualified Data.ByteString.Lazy          as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Servant
import           System.Directory               ( doesFileExist )


--------------------------------------------------------------------------------
-- | The runtime, a central product type that should contain all our runtime
-- supporting values.
data Runtime = Runtime
  { _rConf    :: Command.Conf -- ^ The application configuration.
  , _rDb      :: Data.StmDb Runtime -- ^ The Storage.
  , _rLoggers :: ML.AppNameLoggers -- ^ Multiple loggers to log over.
  }

makeLenses ''Runtime

instance Data.RuntimeHasStmDb Runtime where
  stmDbFromRuntime = _rDb


--------------------------------------------------------------------------------
newtype AppM a = AppM { runAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
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

-- | Run the `AppM` computation catching all possible exceptions.
runAppMSafe
  :: forall a m
   . MonadIO m
  => Runtime
  -> AppM a
  -> m (Either Errs.RuntimeErr a)
runAppMSafe runtime AppM {..} =
  liftIO
    . fmap (join . first Errs.RuntimeException)
    . try @SomeException
    . runExceptT
    $ runReaderT runAppM runtime


--------------------------------------------------------------------------------
-- | Boot up a runtime.
boot
  :: MonadIO m
  => Command.Conf -- ^ configuration to boot with.
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf = do
  _rLoggers <- ML.makeDefaultLoggersWithConf $ _rConf ^. Command.confLogging
  eDb       <- instantiateDb _rConf
  pure $ case eDb of
    Left  err  -> Left err
    Right _rDb -> Right Runtime { .. }

-- | Create a runtime from a given state.
boot' :: MonadIO m => Data.HaskDb Runtime -> FilePath -> m Runtime
boot' db logsPath = do
  let loggingConf = Command.mkLoggingConf logsPath
      _rConf      = Command.defaultConf { Command._confLogging = loggingConf }
  _rDb      <- Data.instantiateStmDb db
  _rLoggers <- ML.makeDefaultLoggersWithConf loggingConf
  pure $ Runtime { .. }

-- | Power down the application: attempting to save the DB state in given file, if possible, and reporting errors otherwise.
powerdown :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
powerdown runtime@Runtime {..} = do
  mDbSaveErr <- saveDb runtime
  -- finally, close loggers.
  ML.flushAndCloseLoggers _rLoggers
  pure mDbSaveErr

saveDb :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
saveDb runtime =
  maybe (pure Nothing) (saveDbAs runtime) $ _rConf runtime ^. Command.confDbFile

saveDbAs :: MonadIO m => Runtime -> FilePath -> m (Maybe Errs.RuntimeErr)
saveDbAs runtime fpath = do
  haskDb <- Data.readFullStmDbInHask $ _rDb runtime
  let bs = Data.serialiseDb haskDb
  liftIO (try @SomeException (BS.writeFile fpath bs))
    <&> either (Just . Errs.RuntimeException) (const Nothing)

{- | Instantiate the db.

1. The state is either the empty db, or if a _confDbFile file is specified, is
read from the file.

2. Whenever the application exits, the state is written to disk, if a
_confDbFile is specified.
-}
instantiateDb
  :: forall m
   . MonadIO m
  => Command.Conf
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
instantiateDb Command.Conf {..} = readDbSafe _confDbFile

readDb
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDb mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if (exists) then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    -- We may want to deserialise the data only when the data is non-empty.
    if BS.null fdata
      then useEmpty
      else
        Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

-- | A safer version of readDb: this fails if the file doesn't exist or is
-- empty. This helps in catching early mistake, e.g. from user specifying the
-- wrong file name on the command-line.
readDbSafe
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDbSafe mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists
      then fromFile fpath
      else pure . Left . Errs.knownErr $ FileDoesntExistErr fpath
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

-- | Natural transformation from some `AppM` in any given mode, to a servant
-- Handler.
appMHandlerNatTrans :: forall a . Runtime -> AppM a -> Servant.Handler a
appMHandlerNatTrans rt appM =
  let
      -- We peel off the AppM + ReaderT layers, exposing our ExceptT
      -- RuntimeErr IO a This is very similar to Servant's Handler:
      -- https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runAppM $ appM
      -- Map our errors to `ServantError`
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in 
      -- Re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT


--------------------------------------------------------------------------------
-- | Support for logging for the application
instance ML.MonadAppNameLogMulti AppM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)


--------------------------------------------------------------------------------
-- | Handle a single command. The @display@ function and the return type
-- provide some flexibility, so this function can be used in both `cty` and
-- `cty repl`.
handleCommand
  :: MonadIO m => Runtime -> (Text -> m ()) -> Command.Command -> m ExitCode
handleCommand runtime display command = do
  case command of
    Command.State -> do
      output <-
        runAppMSafe runtime $ ask >>= Data.readFullStmDbInHaskFromRuntime
      display $ show output
      pure ExitSuccess
    Command.UserCreate input -> do
      output <- runAppMSafe runtime $ withRuntimeAtomically createUser input
      display $ show output
      pure ExitSuccess
    Command.SelectUser select -> do
      output <- runAppMSafe runtime $ S.dbSelect select
      display $ show output
      pure ExitSuccess
    Command.UpdateUser update -> do
      output <- runAppMSafe runtime $ S.dbUpdate update
      display $ show output
      pure ExitSuccess
    _ -> do
      display $ "Unhandled command " <> show command
      return $ ExitFailure 1

-- | Given an initial state, applies a list of commands, returning the list of
-- new (intermediate and final) states.
interpret
  :: Data.HaskDb Runtime -> [Command.Command] -> IO [Data.HaskDb Runtime]
interpret = loop []
 where
  -- TODO Maybe it would be more efficient to thread a runtime instate of a
  -- state (that needs to be "booted" over and over again).
  loop acc _  []               = pure $ reverse acc
  loop acc st (command : rest) = do
    runtime <- boot' st "/tmp/curiosity-xxx.log"
    handleCommand runtime display command
    st' <- Data.readFullStmDbInHask $ _rDb runtime
    loop (st' : acc) st' rest
  display = putStrLn -- TODO Accumulate in a list, so it can be returned.
                     -- TODO Is is possible to also log to a list ?


--------------------------------------------------------------------------------
-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage AppM User.UserProfile where
  dbUpdate = \case

    User.UserCreate input ->
      withRuntimeAtomically createUserFull input
        >>= either Errs.throwError' (pure . pure)

    User.UserCreateGeneratingUserId input -> do
      withRuntimeAtomically createUser input
        >>= either Errs.throwError' (pure . pure)

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
      setPassword =
        set (User.userProfileCreds . User.userCredsPassword) newPass
      replaceOlder users =
        [ if S.dbId u == id then setPassword u else u | u <- users ]

  dbSelect = \case

    User.UserLoginWithUserName input -> do
      withRuntimeAtomically checkCredentials input
        >>= either Errs.throwError' (pure . pure)

    User.SelectUserById id ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== id) . S.dbId)

    User.SelectUserByUserName username ->
      toList <$> withRuntimeAtomically selectUserByUsername username

modifyUserProfiles id f userProfiles =
  liftIO $ STM.atomically (STM.modifyTVar userProfiles f) $> [id]

selectUserById runtime id = do
  let usersTVar = Data._dbUserProfiles $ _rDb runtime
  STM.readTVar usersTVar <&> find ((== id) . S.dbId)

selectUserByUsername
  :: Runtime -> User.UserName -> STM (Maybe User.UserProfile)
selectUserByUsername runtime username = do
  let usersTVar = Data._dbUserProfiles $ _rDb runtime
  users' <- STM.readTVar usersTVar
  pure $ find ((== username) . User._userCredsName . User._userProfileCreds)
              users'

createUser
  :: Runtime
  -> User.Signup
  -> STM (Either User.UserErr User.UserId)
createUser runtime User.Signup {..} = do
  newId <- generateUserId runtime
  let newProfile = User.UserProfile newId
                                    (User.Credentials username password)
                                    "TODO"
                                    email
                                    tosConsent
  createUserFull runtime newProfile

createUserFull
  :: Runtime -> User.UserProfile -> STM (Either User.UserErr User.UserId)
createUserFull runtime newProfile =
  if username `elem` User.usernameBlocklist
    then
      pure . Left $ User.UsernameBlocked
    else do
      mprofile <- selectUserById runtime newProfileId
      case mprofile of
        Just profile -> existsErr
        Nothing      -> createNew
 where
  username = newProfile ^. User.userProfileCreds . User.userCredsName
  newProfileId = S.dbId newProfile
  createNew    = do
    mprofile <- selectUserByUsername
      runtime
      username
    case mprofile of
      Just profile -> existsErr
      Nothing      -> do
        modifyUsers runtime (newProfile :)
        pure $ Right newProfileId
  existsErr = pure . Left $ User.UserExists

generateUserId :: Runtime -> STM (User.UserId)
generateUserId runtime = do
  let nextUserIdTVar = Data._dbNextUserId $ _rDb runtime
  STM.stateTVar nextUserIdTVar (\i -> (User.UserId $ "USER-" <> show i, succ i))

modifyUsers :: Runtime -> ([User.UserProfile] -> [User.UserProfile]) -> STM ()
modifyUsers runtime f = do
  let usersTVar = Data._dbUserProfiles $ _rDb runtime
  STM.modifyTVar usersTVar f

checkCredentials
  :: Runtime -> User.Credentials -> STM (Either User.UserErr User.UserProfile)
checkCredentials runtime User.Credentials {..} = do
  mprofile <- selectUserByUsername runtime _userCredsName
  case mprofile of
    Just profile | checkPassword profile _userCredsPassword ->
      pure $ Right profile
    _ -> pure $ Left User.IncorrectUsernameOrPassword

-- TODO Use constant-time string comparison.
checkPassword :: User.UserProfile -> User.Password -> Bool
checkPassword profile (User.Password passInput) = storedPass =:= passInput
 where
  User.Password storedPass =
    profile ^. User.userProfileCreds . User.userCredsPassword

onUserIdExists id onNone onExisting =
  S.dbSelect (User.SelectUserById id) <&> headMay >>= maybe onNone onExisting

userNotFound =
  Errs.throwError' . User.UserNotFound . mappend "User not found: "

withRuntimeAtomically f a = ask >>= \rt -> liftIO . STM.atomically $ f rt a

withUserStorage f = asks (Data._dbUserProfiles . _rDb) >>= f


--------------------------------------------------------------------------------
instance S.DBStorage AppM Todo.TodoList where

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


--------------------------------------------------------------------------------
newtype IOErr = FileDoesntExistErr FilePath
  deriving Show

instance Errs.IsRuntimeErr IOErr where
  errCode FileDoesntExistErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus FileDoesntExistErr{} = HTTP.notFound404
  userMessage = Just . \case
    FileDoesntExistErr fpath -> T.unwords ["File doesn't exist:", T.pack fpath]
