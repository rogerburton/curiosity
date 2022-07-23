{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import           Data.List                      ( words )
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import qualified System.Console.Haskeline      as HL


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-repl-2 - Curiosity REPL"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)

  handleExceptions $ do
    repl runtime
    P.shutdown runtime Nothing


--------------------------------------------------------------------------------
repl :: Rt.Runtime -> IO ()
repl runtime = HL.runInputT HL.defaultSettings loop
 where
  loop = HL.getInputLine prompt >>= \case
    Nothing     -> output' ""
    -- TODO Probably processInput below (within parseAnyStateInput) should have
    -- other possible results (beside mod and viz): comments and blanks
    -- (no-op), instead of this special empty case.
    Just ""     -> loop
    Just "quit" -> pure ()
    Just input  -> do
      let result =
            A.execParserPure A.defaultPrefs Command.parserInfo $ words input
      case result of
        A.Success command ->
          Rt.handleCommand runtime output' command >> pure ()
        A.Failure           err -> output' $ show err
        A.CompletionInvoked _   -> output' "Shouldn't happen"

      loop

  prompt  = "> "

  output' = HL.outputStrLn . T.unpack
