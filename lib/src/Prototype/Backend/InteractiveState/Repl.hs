{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Prototype.Backend.InteractiveState.Repl
  ( ReplConf(..)
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
import qualified System.Console.Readline       as RL

data ReplConf = ReplConf
  { _replPrompt       :: Text
  , _replHistory      :: Bool
  , _replReplExitCmds :: [Text]
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
         in  do

               RL.addHistory cmdString
               output <- runMInIO $ processInput replInput

               case output of
                 IS.ReplOutputStrict txt  -> output' txt
                 IS.ReplOutputLazy   txtL -> output' $ TL.toStrict txtL
                 IS.ReplRuntimeErr   err  -> output' $ Errs.displayErr err
               pure ReplContinue
     where
      isReplExit =
        cmd `elem` _replReplExitCmds || (T.strip cmd) `elem` _replReplExitCmds
      cmd = T.pack cmdString

  loopRepl exit@ReplExitOnUserCmd{}          = pure exit
  loopRepl exit@ReplExitOnGeneralException{} = pure exit

  output' txt = RL.getOutStream >>= (`T.IO.hPutStrLn` txt)

  mapSomeEx op =
    try @SomeException op <&> either ReplExitOnGeneralException identity

  prompt = T.unpack _replPrompt

