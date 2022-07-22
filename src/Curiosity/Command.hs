{-# LANGUAGE DataKinds #-}
module Curiosity.Command
  ( handleCommand
  ) where

import qualified Commence.InteractiveState.Class
                                               as IS
import qualified Curiosity.Data                as Data
import qualified Curiosity.Parse2              as P
import qualified Curiosity.Runtime             as Rt


--------------------------------------------------------------------------------
-- | Handle a single command. The @display@ function and the return type
-- provide some flexibility, so this function can be used in both `cty` and
-- `cty-repl-2`.
handleCommand :: MonadIO m => Rt.Runtime -> (Text -> m ()) -> P.Command -> m ExitCode
handleCommand runtime display command = do
  case command of
    P.State -> do
      output <-
        Rt.runAppMSafe runtime
        . IS.execVisualisation
        $ Data.VisualiseFullStmDb
      display $ show output
      return ExitSuccess
    P.SelectUser select -> do
      output <-
        Rt.runAppMSafe runtime . IS.execVisualisation $ Data.VisualiseUser
          select
      display $ show output
      return ExitSuccess
    P.UpdateUser update -> do
      output <-
        Rt.runAppMSafe runtime . IS.execModification $ Data.ModifyUser
          update
      display $ show output
      return ExitSuccess
    _ -> do
      display $ "Unhandled command " <> show command
      return $ ExitFailure 1
