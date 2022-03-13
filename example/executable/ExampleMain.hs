{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import           Control.Lens
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

main :: IO ExitCode
main =
  putStrLn @Text "Parsing opts." >> A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info P.confParser
    $  A.fullDesc
    <> A.header "Prototype-hs Example program"
    <> A.progDesc
         "Interactive state demo: modify states via multiple sources of input: HTTP and a REPL."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  -- The first step is to boot up a runtime. 
  putStrLn @Text "boot runtime."
  jwk     <- Srv.generateKey
  runtime <- Rt.boot conf Nothing jwk >>= either throwIO pure
  putStrLn @Text "booted runtime."
  replProcess runtime `concurrently` serverProcess runtime
  -- FIXME: correct exit codes based on exit reason.
  exitSuccess
 where
  replProcess =
    startRepl >=> putStrLn @Text . mappend "REPL process ended: " . show
  serverProcess =
    startServer
      >=> putStrLn @Text
      .   mappend "Server process ended: "
      .   Errs.displayErr

startRepl :: Rt.Runtime -> IO Repl.ReplLoopResult
startRepl rt = runSafeMapErrs $ Repl.startReadEvalPrintLoop
  (rt ^. Rt.rConf . Rt.confRepl)
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

-- FIXME: Implement the server part; currently its just a forever running loop.
startServer :: Rt.Runtime -> IO Errs.RuntimeErr
startServer rt = try @SomeException (Srv.runExampleServer rt) >>= pure . either
  Errs.RuntimeException
  (const $ Errs.RuntimeException UserInterrupt) -- FIXME: improve this, incorrect error reporting here. 
 where


