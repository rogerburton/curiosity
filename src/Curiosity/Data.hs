{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies   #-}
module Curiosity.Data
  ( Db(..)
  , StmDb
  , HaskDb
  -- * Constraints
  , RuntimeHasStmDb(..)
  -- * Instantiating databases. 
  , emptyHask
  , instantiateStmDb
  , instantiateEmptyStmDb
  -- * Reading values from the database.
  , readFullStmDbInHaskFromRuntime
  , readFullStmDbInHask
  -- * Serialising and deseralising DB to bytes.
  , serialiseDb
  , deserialiseDb
  ) where

import qualified Commence.Runtime.Errors       as E
import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Data.Todo           as Todo
import qualified Curiosity.Data.User           as U
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Network.HTTP.Types.Status     as S

{- | The central database. The product type contains all values and is parameterised by @datastore@. The @datastore@ can be the layer
dealing with storage. When it is @Identity@, it just means the data is stored as is. It can, however, also be an `STM.TVar` if the datastore is to be
STM based. 

Additionally, we want to parameterise over a @runtime@ type parameter. This is a container type of the database. 
-}
data Db (datastore :: Type -> Type) (runtime :: Type) = Db
  { _dbNextUserId   :: datastore Int
  , _dbUserProfiles :: datastore [U.UserProfile]
  , _dbTodos        :: datastore [Todo.TodoList]
  }

-- | Hask database type: used for starting the system, values reside in @Hask@ (thus `Identity`)
type HaskDb runtime = Db Identity runtime

deriving instance Eq (HaskDb runtime)
deriving instance Show (HaskDb runtime)
deriving instance Generic (HaskDb runtime)
deriving anyclass instance ToJSON (HaskDb runtime)
deriving anyclass instance FromJSON (HaskDb runtime)

-- | Stm database type, used for live example applications, values reside in @STM@  
type StmDb runtime = Db STM.TVar runtime

-- | Instantiate a seed database that is empty.
emptyHask :: forall runtime . HaskDb runtime
emptyHask = Db (pure 1) (pure mempty) (pure mempty)

instantiateStmDb
  :: forall runtime m . MonadIO m => HaskDb runtime -> m (StmDb runtime)
instantiateStmDb Db { _dbNextUserId = Identity seedNextUserId, _dbUserProfiles = Identity seedProfiles, _dbTodos = Identity seedTodos }
  =
  -- We don't use `newTVarIO` repeatedly under here and instead wrap the whole
  -- instantiation under a single STM transaction (@atomically@).
    liftIO . STM.atomically $ do
    _dbNextUserId   <- STM.newTVar seedNextUserId
    _dbUserProfiles <- STM.newTVar seedProfiles
    _dbTodos        <- STM.newTVar seedTodos
    pure Db { .. }

instantiateEmptyStmDb :: forall runtime m . MonadIO m => m (StmDb runtime)
instantiateEmptyStmDb = instantiateStmDb emptyHask

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHaskFromRuntime
  :: forall runtime m
   . (MonadIO m, RuntimeHasStmDb runtime)
  => runtime
  -> m (HaskDb runtime)
readFullStmDbInHaskFromRuntime = readFullStmDbInHask . stmDbFromRuntime
{-# INLINE readFullStmDbInHaskFromRuntime #-}

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHask
  :: forall runtime m . MonadIO m => StmDb runtime -> m (HaskDb runtime)
readFullStmDbInHask stmDb = liftIO . STM.atomically $ do
  _dbNextUserId   <- pure <$> STM.readTVar (_dbNextUserId stmDb)
  _dbUserProfiles <- pure <$> STM.readTVar (_dbUserProfiles stmDb)
  _dbTodos        <- pure <$> STM.readTVar (_dbTodos stmDb)
  pure Db { .. }


{- | Provides us with the ability to constrain on a larger product-type (the @runtime@) to contain, in some form or another, a value
of the `StmDb`, which can be accessed from the @runtime@.

This solves cyclic imports, without caring about the concrete @runtime@ types, we can just rely on the constraints. 
-}
class RuntimeHasStmDb runtime where
  stmDbFromRuntime :: runtime -> StmDb runtime

newtype DbErr = DbDecodeFailed Text
              deriving Show

instance E.IsRuntimeErr DbErr where
  errCode = errCode' . \case
    DbDecodeFailed{} -> "DECODE_FAILED"
    where errCode' = mappend "ERR.DB."
  httpStatus = \case
    DbDecodeFailed{} -> S.internalServerError500
  userMessage = Just . \case
    DbDecodeFailed msg -> msg

-- | Write an entire db state to bytes. 
serialiseDb :: forall runtime . HaskDb runtime -> LByteString
serialiseDb = encode
{-# INLINE serialiseDb #-}

-- | Read an entire db state from bytes. 
deserialiseDb :: forall runtime . LByteString -> Either DbErr (HaskDb runtime)
deserialiseDb = first (DbDecodeFailed . T.pack) . eitherDecode
{-# INLINE deserialiseDb #-}
