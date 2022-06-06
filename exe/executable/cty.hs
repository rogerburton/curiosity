-- | This is the main server-side program to interact with the server.

{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Exe.Data            as Data
import qualified Prototype.Exe.Exe.Parse       as P
import qualified Prototype.Exe.Exe.Parse2      as P
import qualified Prototype.Exe.Runtime         as Rt
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser P.parserInfoWithTarget >>= run


--------------------------------------------------------------------------------
run :: P.CommandWithTarget -> IO ExitCode
run (P.CommandWithTarget command target) = do
  case target of
    P.StateFileTarget path -> do
      jwt                     <- Srv.generateKey
      runtime@Rt.Runtime {..} <-
        Rt.boot P.defaultConf { Rt._confDbFile = Just path } jwt
          >>= either throwIO pure
      -- TODO jwt should'nt be in the runtime, but in the HTTP layer

      case command of
        P.SelectUser select -> do
          output <-
            Rt.runExeAppMSafe runtime
            . IS.execVisualisation
            $ Data.VisualiseUser select
          print output
        P.UpdateUser update -> do
          output <-
            Rt.runExeAppMSafe runtime . IS.execModification $ Data.ModifyUser
              update
          print output
        _ -> do
          putStrLn @Text $ "Unhandled command " <> show command
          exitFailure

      Rt.powerdown runtime
      -- TODO shutdown runtime, loggers, save state, ...
      exitSuccess

    P.UnixDomainTarget _ -> do
      putStrLn @Text "Unimplemented: --socket, a.k.a UnixDomainTarget"
      exitFailure
