{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Data.Text                     as T
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo P.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-run - Curiosity script interpreter"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: P.Conf -> IO ExitCode
runWithConf conf = do
  runtime <- Rt.boot conf >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)

  handleExceptions $ do
    interpret runtime "scenarios/example.txt"
    P.shutdown runtime Nothing


--------------------------------------------------------------------------------
interpret :: Rt.Runtime -> FilePath -> IO ()
interpret runtime path = do
  content <- readFile path
  loop $ T.lines content
 where
  loop []            = pure ()
  loop (line : rest) = case T.words line of
    []       -> loop rest
    ["quit"] -> pure ()
    input    -> do
      let result = A.execParserPure A.defaultPrefs Command.parserInfo
            $ map T.unpack input
      case result of
        A.Success command -> do
          Rt.handleCommand runtime output' command
          loop rest
        A.Failure err -> do
          output' $ show err
          exitFailure
        A.CompletionInvoked _ -> do
          output' "Shouldn't happen"
          exitFailure

  output' = putStrLn . T.unpack
