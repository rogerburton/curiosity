{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module ExampleMain
  ( main
  ) where

import "base"    Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently )
import           Control.Lens
import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Exe.Parse   as P
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Runtime.Errors      as Errs

main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

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
  runtime                    <- Rt.boot conf Nothing >>= either throwIO pure
  (replResult, serverResult) <- startRepl runtime
    `concurrently` startServer runtime
  replExitedWith replResult
  serverExitedWith serverResult
  -- FIXME: correct exit codes based on exit reason.
  exitSuccess
 where
  replExitedWith = putStrLn @Text . mappend "Repl exited with: " . show
  serverExitedWith =
    putStrLn @Text . mappend "Server exited with: " . Errs.displayErr

startRepl :: Rt.Runtime -> IO Repl.ReplLoopResult
startRepl rt = undefined $ Repl.startReadEvalPrintLoop
  (rt ^. Rt.rConf . Rt.confRepl)
  (handleReplInputs :: (  IS.DispInput 'IS.Repl
    -> Rt.ExampleAppM (IS.DispOutput 'IS.Repl)
    )
  )
  (undefined :: (Rt.ExampleAppM a -> IO a))
 where
  handleReplInputs =
    IS.execAnyInputOnState @(Data.StmDb Rt.Runtime) @ 'IS.Repl @Rt.ExampleAppM
      >=> either displayErr displayOutput
  displayErr    = undefined
  displayOutput = undefined


-- FIXME: Implement the server part; currently its just a forever running loop.
startServer :: Rt.Runtime -> IO Errs.RuntimeErr
startServer rt = (`catch` handleRuntime) $ do
  threadDelay secs10
  startServer rt
 where
  handleRuntime = pure
  secs10        = 10 * 10 ^ (6 :: Int)
