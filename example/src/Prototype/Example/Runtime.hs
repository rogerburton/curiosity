{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prototype.Example.Runtime
  ( Conf(..)
  , Runtime(..)
  , ExampleAppM(..)
  ) where

import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Data.User   as Data.User
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S

data Conf

-- | The runtime, a central product type that should contain all our runtime supporting values. 
data Runtime = Runtime
  { _rConf :: Conf -- ^ The application configuration.
  , _rDb   :: Data.StmDb Runtime -- ^ The Storage. 
  }

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
instance S.DBStorage ExampleAppM Data.User.UserProfile where
  dbUpdate = undefined
  dbSelect = undefined
