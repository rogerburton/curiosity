{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Prototype.Backend.InteractiveState.Repl
  ( ReplConf(..)
  , ExitCmd(..)
  , ReplLoopResult(..)
  , startReadEvalPrintLoop
  ) where

import           Data.Default.Class
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text.Lazy                as TL
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Runtime.Errors      as Errs
import           Prototype.Types.NonEmptyText
import           PrototypeHsLibPrelude
import qualified System.Console.Haskeline      as HL

-- | Newtype over exit commands for type-safety.
newtype ExitCmd = ExitCmd NonEmptyText
                deriving (Eq, Show, IsString) via NonEmptyText

data ReplConf = ReplConf
  { _replPrompt       :: Text
    -- ^ A prompt for the REPL, must not include trailing spaces.
  , _replHistory      :: Bool
    -- ^ Do we want to have repl-history? TODO: Currently unused.
  , _replReplExitCmds :: [ExitCmd]
    -- ^ The list of exit commands we want to exit with
  }
  deriving Show

instance Default ReplConf where
  def = ReplConf "% " True ["exit"]

data ReplLoopResult where
  -- | Some fatal exception that caused the repl exit; more like a catch-all.
  ReplExitOnGeneralException ::Exception e => e -> ReplLoopResult
  -- | The user explicitly indicated they'd like to exit the repl.
  ReplExitOnUserCmd ::Text -> ReplLoopResult

deriving instance Show ReplLoopResult

startReadEvalPrintLoop
  :: forall m
   . MonadIO m
  => ReplConf
     -- ^ The configuration to run with.
  -> (IS.DispInput 'IS.Repl -> m (IS.DispOutput 'IS.Repl))
     -- ^ Function mapping the repl input to output.
  -> (forall a . m a -> IO (Either Errs.RuntimeErr a))
     -- ^ Instructions on how to run some @m@ into @IO@
  -> m ReplLoopResult
     -- ^ The reason for user-exit.
startReadEvalPrintLoop ReplConf {..} processInput runMInIO =
  liftIO . mapSomeEx $ HL.runInputT HL.defaultSettings loop
 where
  mapSomeEx op =
    try @SomeException op <&> either ReplExitOnGeneralException identity

  loop = HL.getInputLine prompt >>= \case
    Nothing                       -> output' "" $> ReplExitOnUserCmd "eof"
    -- TODO Probably processInput below (within parseAnyStateInput) should have
    -- other possible results (beside mod and viz): comments and blanks
    -- (no-op), instead of this special empty case.
    Just ""                       -> loop
    Just input | isReplExit input -> pure . ReplExitOnUserCmd $ T.pack input
    Just input                    -> do
      let input' = IS.ReplInputStrict $ T.pack input
      output <- liftIO . runMInIO $ processInput input'

      case output of
        Right (IS.ReplOutputStrict txt ) -> output' txt
        Right (IS.ReplOutputLazy   txtL) -> output' $ TL.toStrict txtL
        Right (IS.ReplRuntimeErr   err ) -> output' $ Errs.displayErr err
        Left rtErr ->
          output' $ "Unhandled runtime error: " <> Errs.displayErr rtErr

      loop

  prompt  = T.unpack _replPrompt

  output' = HL.outputStrLn . T.unpack

  isReplExit txt = case nonEmptyText $ T.pack txt of
    Nothing    -> False
    Just neCmd -> ExitCmd neCmd `elem` _replReplExitCmds
