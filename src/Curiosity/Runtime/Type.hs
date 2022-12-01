{-# LANGUAGE TemplateHaskell #-}
module Curiosity.Runtime.Type
  ( Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , rThreads
  , Threads(..)
  ) where

import qualified Commence.Multilogging         as ML
import           Control.Lens
import qualified Curiosity.Core                as Core
import qualified Curiosity.Parse               as Parse


--------------------------------------------------------------------------------
-- | The runtime, a central product type that should contain all our runtime
-- supporting values: the STM state, loggers, and processing threads.
data Runtime = Runtime
  { _rConf    :: Parse.Conf -- ^ The application configuration.
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
    , _tUnixThread :: MVar ThreadId -- ^ UNIX-domain socket server thread.
    }

makeLenses ''Runtime
