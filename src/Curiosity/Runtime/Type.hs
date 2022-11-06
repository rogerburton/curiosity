{-# LANGUAGE TemplateHaskell #-}
module Curiosity.Runtime.Type
  ( Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , Threads(..)
  -- * managing runtimes: boot, shutdown etc. 
  , boot
  , boot'
  , instantiateDb
  , readDb
  , readDbSafe
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Control.Concurrent.STM        as STM
import           Control.Lens
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import qualified Curiosity.Parse               as Command
import qualified Curiosity.Runtime.Error       as RErr
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as T
import           System.Directory               ( doesFileExist )


--------------------------------------------------------------------------------
-- | The runtime, a central product type that should contain all our runtime
-- supporting values: the STM state, loggers, and processing threads.
data Runtime = Runtime
  { _rConf    :: Command.Conf -- ^ The application configuration.
  , _rDb      :: Core.StmDb -- ^ The Storage.
  , _rLoggers :: ML.AppNameLoggers -- ^ Multiple loggers to log over.
  , _rThreads :: Threads -- ^ Additional threads running e.g. async tasks.
  }

-- | Describes the threading configuration: what the main thread is, and what
-- additional threads can be created.
data Threads =
    NoThreads
    -- ^ Means that threads can't be started or stopped dynamically.
  | ReplThreads
    -- ^ Means the main thread is the REPL, started with `cty repl`.
    { _tEmailThread :: MVar ThreadId
    }
  | HttpThreads
    -- ^ Means the main thread is the HTTP server, started with `cty serve`.
    { _tEmailThread :: MVar ThreadId
    }

makeLenses ''Runtime


--------------------------------------------------------------------------------
-- | Boot up a runtime.
boot
  :: MonadIO m
  => Command.Conf -- ^ configuration to boot with.
  -> Threads
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf _rThreads =
  liftIO
      (  try @SomeException
      .  ML.makeDefaultLoggersWithConf
      $  _rConf
      ^. Command.confLogging
      )
    >>= \case
          Left loggerErrs ->
            putStrLn @Text
                (  "Cannot instantiate, logger instantiation failed: "
                <> show loggerErrs
                )
              $> Left (Errs.RuntimeException loggerErrs)
          Right _rLoggers -> do
            eDb <- instantiateDb _rConf
            pure $ case eDb of
              Left  err  -> Left err
              Right _rDb -> Right Runtime { .. }

-- | Create a runtime from a given state.
boot' :: MonadIO m => Data.HaskDb -> FilePath -> m Runtime
boot' db logsPath = do
  let loggingConf = Command.mkLoggingConf logsPath
      _rConf      = Command.defaultConf { Command._confLogging = loggingConf }
  _rDb      <- liftIO . STM.atomically $ Core.instantiateStmDb db
  _rLoggers <- ML.makeDefaultLoggersWithConf loggingConf
  let _rThreads = NoThreads
  pure $ Runtime { .. }

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
  -> m (Either Errs.RuntimeErr Core.StmDb)
instantiateDb Command.Conf {..} = readDbSafe _confDbFile

readDb
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr Core.StmDb)
readDb mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath = liftIO (try @SomeException $ T.readFile fpath) >>= \case
    Left err ->
      putStrLn @Text ("Unable to read db file: " <> maybe "" T.pack mpath)
        $> Left (Errs.RuntimeException err)
    Right fdata ->
           -- We may want to deserialise the data only when the data is non-empty.
                   if T.null fdata
      then useEmpty
      else
        Data.deserialiseDbStrict (TE.encodeUtf8 fdata)
          & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> liftIO (STM.atomically Core.instantiateEmptyStmDb)
  useState = fmap Right . liftIO . STM.atomically . Core.instantiateStmDb

-- | A safer version of readDb: this fails if the file doesn't exist or is
-- empty. This helps in catching early mistake, e.g. from user specifying the
-- wrong file name on the command-line.
readDbSafe
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr Core.StmDb)
readDbSafe mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists
      then fromFile fpath
      else pure . Left . Errs.knownErr $ RErr.FileDoesntExistErr fpath
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (T.readFile fpath)
    Data.deserialiseDbStrict (TE.encodeUtf8 fdata)
      & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> liftIO (STM.atomically Core.instantiateEmptyStmDb)
  useState = fmap Right . liftIO . STM.atomically . Core.instantiateStmDb
