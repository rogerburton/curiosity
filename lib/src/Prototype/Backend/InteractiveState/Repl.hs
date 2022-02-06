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
import           Prelude
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Runtime.Errors      as Errs
import           Prototype.Types.NonEmptyText
import qualified System.Console.Readline       as RL

-- | Newtype over exit commands for type-safety.
newtype ExitCmd = ExitCmd NonEmptyText
                deriving (Eq, Show, IsString) via NonEmptyText

data ReplConf = ReplConf
  { _replPrompt       :: Text -- ^ A prompt for the REPL, must not include trailing spaces. 
  , _replHistory      :: Bool  -- ^ Do we want to have repl-history? 
  , _replReplExitCmds :: [ExitCmd] -- ^ The list of exit commands we want to exit with
  }
  deriving Show

instance Default ReplConf where
  def = ReplConf "% " True ["exit"]

data ReplLoopResult where
  -- | Some fatal exception that caused the repl exit; more like a catch-all. 
  ReplExitOnGeneralException ::Exception e => e -> ReplLoopResult
  -- | The user explicitly indicated they'd like to exit the repl.
  ReplExitOnUserCmd ::Text -> ReplLoopResult
  -- | Continue the repl loop.
  ReplContinue ::ReplLoopResult

deriving instance Show ReplLoopResult

-- | Start up the REPL.
startReadEvalPrintLoop
  :: forall m
   . MonadIO m
  => ReplConf -- ^ The configuration to run with.
  -> (IS.DispInput 'IS.Repl -> m (IS.DispOutput 'IS.Repl)) -- ^ Function mapping the repl input to output.
  -> (forall a . m a -> IO a) -- ^ Instructions on how to run some @m@ into @IO@
  -> m ReplLoopResult -- ^ The reason for user-exit. 
startReadEvalPrintLoop ReplConf {..} processInput runMInIO =
  liftIO . mapSomeEx $ loopRepl ReplContinue
 where
  loopRepl ReplContinue = RL.readline prompt >>= \case
    Nothing -> loopRepl ReplContinue
    Just cmdString
      | isReplExit
      -> pure $ ReplExitOnUserCmd cmd
      | otherwise
      -> let replInput = IS.ReplInputStrict cmd
         in 
          -- Add the input to the history, process it, and write the output to the output stream.
             do

               when _replHistory $ RL.addHistory cmdString

               -- Process input.
               output <- runMInIO $ processInput replInput

               case output of
                 IS.ReplOutputStrict txt  -> output' txt
                 IS.ReplOutputLazy   txtL -> output' $ TL.toStrict txtL
                 IS.ReplRuntimeErr   err  -> output' $ Errs.displayErr err

               pure ReplContinue
     where
      isReplExit = case nonEmptyText cmd of
        Nothing    -> False
        Just neCmd -> ExitCmd neCmd `elem` _replReplExitCmds
      cmd = T.strip . T.pack $ cmdString

  loopRepl exit@ReplExitOnUserCmd{}          = pure exit
  loopRepl exit@ReplExitOnGeneralException{} = pure exit

  output' txt = RL.getOutStream >>= (`T.IO.hPutStrLn` txt)

  mapSomeEx op =
    try @SomeException op <&> either ReplExitOnGeneralException identity

  prompt = T.unpack _replPrompt

