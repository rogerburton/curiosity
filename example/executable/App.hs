{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import           Control.Lens
import qualified Control.Monad.Log             as L
import qualified Data.Text                     as T
import qualified MultiLogging                  as ML
import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Exe.Parse   as P
import qualified Prototype.Example.Exe.Process as P
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Example.Server      as Srv
import qualified Prototype.Runtime.Errors      as Errs
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "Curiosity"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  jwk                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf Nothing jwk >>= either throwIO pure

  P.startServer runtime >>= P.endServer _rLoggers

  -- Close all loggers.
  ML.flushAndCloseLoggers _rLoggers

  -- FIXME: correct exit codes based on exit reason.
  exitSuccess
