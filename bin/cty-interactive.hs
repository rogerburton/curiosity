{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import qualified Commence.InteractiveState.Repl
                                               as Repl
import           Commence.Multilogging          ( flushAndCloseLoggers )
import qualified Control.Concurrent.Async      as Async
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server              as Srv
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ExitCode
main =
  A.execParser mainParserInfo
    >>= runWithConf

mainParserInfo :: A.ParserInfo (Rt.Conf, Srv.ServerConf, Repl.ReplConf)
mainParserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-interactive - Curiosity HTTP server and REPL"
    <> A.progDesc
         "Interactive state demo: modify states via multiple sources of input: \
         \HTTP and a REPL."
 where
  parser = (,,) <$> P.confParser <*> P.serverParser <*> P.replParser

runWithConf :: (Rt.Conf, Srv.ServerConf, Repl.ReplConf) -> IO ExitCode
runWithConf (conf, serverConf, replConf) = do
  putStrLn @Text
    "Booting runtime; the rest of the startup logs will be in the configured logging outputs."
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)
      reportServerEnd  = P.startupLogInfo
        _rLoggers
        "Shutting down repl since server process exited."
      reportReplEnd = P.startupLogInfo _rLoggers "Shutting down the repl."

  handleExceptions
    -- the server and repl processes are different: for the server, we are conserving the exception with which the the server process exited.
    -- this exception is also used to end the repl process.
    $ let serverProcess = do
            err <- P.startServer serverConf runtime
            P.endServer _rLoggers err
            -- re-report the error. 
            pure err

          replProcess = P.startRepl replConf runtime >>= P.endRepl
      in  Async.withAsync serverProcess $ \serverRef -> do
            Async.withAsync replProcess $ \replRef -> do
              -- wait for the server process to exit. 
              serverErr <- Async.wait serverRef
              reportServerEnd
              -- Since the server exited with an exception, we'll also want to shut down the repl. 
              Async.cancelWith replRef serverErr
              -- wait for the repl process to exit.
              Async.wait replRef
              reportReplEnd
              -- Close loggers. 
              flushAndCloseLoggers _rLoggers
              exitFailure


