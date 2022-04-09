{-# LANGUAGE ApplicativeDo #-}
module Prototype.Example.Exe.Parse
  ( confParser
  ) where

import           Data.Default.Class
import qualified MultiLogging                  as ML
import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import           Prototype.Example.Runtime

confParser :: A.Parser Conf
confParser = do
  _confServer  <- serverParser
  _confRepl    <- replParser
  _confLogging <- ML.parseLoggingConf
  pure Conf { .. }
serverParser = ServerConf <$> A.option
  A.auto
  (A.long "server-port" <> A.metavar "PORT" <> A.help
    "Port to run the HTTP server on."
  )
replParser = do
  _replPrompt <-
    A.strOption
    $  A.long "repl-prompt"
    <> A.value (Repl._replPrompt def)
    <> A.showDefault
    <> A.metavar "PROMPT"
    <> A.help "Prompt to use for the repl"
  _replHistory <- A.switch $ A.long "repl-history-on" <> A.help
    "Flag to enable history."
  _replReplExitCmds <- A.many $ A.strOption (A.long "repl-exit-cmd")
  pure Repl.ReplConf { .. }

