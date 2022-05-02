{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import qualified Control.Concurrent.Async      as Async
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
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Example.Server      as Srv
import qualified Prototype.Runtime.Errors      as Errs
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ExitCode
main =
  putStrLn @Text "Parsing command-line options..."
    >>  A.execParser mainParserInfo
    >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "Prototype-hs Example program"
    <> A.progDesc
         "Interactive state demo: modify states via multiple sources of input: \
         \HTTP and a REPL."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  putStrLn @Text
    "Booting runtime; the rest of the startup logs will be in the configured logging outputs."
  jwk                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf Nothing jwk >>= either throwIO pure

  let handleExceptions = (`catch` shutdown runtime . Just)

  handleExceptions $ do
    (startServer runtime >>= endServer _rLoggers)
      `Async.concurrently_` (startRepl runtime >>= endRepl)
    shutdown runtime Nothing

shutdown :: Rt.Runtime -> Maybe SomeException -> IO ExitCode
shutdown Rt.Runtime {..} mException = do
  startupLogInfo _rLoggers
    $  "Shutting down: "
    <> maybe "graceful exit" show mException
    <> "."
  ML.flushAndCloseLoggers _rLoggers
  if isJust mException then exitFailure else exitSuccess

--------------------------------------------------------------------------------
startRepl :: Rt.Runtime -> IO Repl.ReplLoopResult
startRepl rt@Rt.Runtime {..} = runSafeMapErrs $ do
  startupLogInfo _rLoggers "Starting up REPL..."
  Repl.startReadEvalPrintLoop (rt ^. Rt.rConf . Rt.confRepl)
                              handleReplInputs
                              (Rt.runExampleAppMSafe rt)
 where
  handleReplInputs =
    -- TypeApplications not needed below, but left for clarity.
    IS.execAnyInputOnState @(Data.StmDb Rt.Runtime) @ 'IS.Repl @Rt.ExampleAppM
      >=> either displayErr pure
  runSafeMapErrs = fmap (either Repl.ReplExitOnGeneralException identity)
    . Rt.runExampleAppMSafe rt
  displayErr (Data.ParseFailed err) =
    pure . IS.ReplOutputStrict . Errs.displayErr $ err

endRepl :: Repl.ReplLoopResult -> IO ()
endRepl res = putStrLn @Text $ T.unlines ["REPL process ended: " <> show res]

--------------------------------------------------------------------------------
startServer :: Rt.Runtime -> IO Errs.RuntimeErr
startServer runtime@Rt.Runtime {..} = do
  let Rt.ServerConf port = runtime ^. Rt.rConf . Rt.confServer
  startupLogInfo _rLoggers $ "Starting up server on port " <> show port <> "..."
  try @SomeException (Srv.runExampleServer runtime) >>= pure . either
    Errs.RuntimeException
    (const $ Errs.RuntimeException UserInterrupt)
  -- FIXME: improve this, incorrect error reporting here.

endServer :: ML.AppNameLoggers -> Errs.RuntimeErr -> IO ()
endServer loggers =
  startupLogInfo loggers . mappend "Server process ended: " . Errs.displayErr

{- | Startup logging using standard loggers instead of using the putStrLn blindly.
`putStrLn` may cause issues with the REPL since both rely on STDOUT. 

The implementation is simple: if there are no loggers, we don't output anything. However, if there is one, we log on the first logger.

FIXME: check if the logger is not using STDOUT, or, find the first non-STDOUT logger and log on that. 
-}
startupLogInfo :: MonadIO m => ML.AppNameLoggers -> Text -> m ()
startupLogInfo (ML.AppNameLoggers loggers) msg = mapM_ logOver loggers
  where logOver l = L.runLogT' l . L.localEnv (<> "Boot") $ L.info msg
