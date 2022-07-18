-- | This program parses command intended for the REPL and displays their
-- internal representations.
--
-- Usage:
--   cty-parse -e "viz all"
--   cty-parse <filename>
--   echo "viz all" | cty-parse -

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import qualified Commence.InteractiveState.Class
                                               as IS
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import qualified Prototype.Data                as Data
import qualified Prototype.Runtime             as Rt


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser parserInfo >>= run

data Conf =
    ConfCommand Text
  | ConfFileName FilePath
  | ConfStdin

parserInfo :: A.ParserInfo Conf
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "Curiosity parser"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

parser :: A.Parser Conf
parser = parserCommand <|> parserFileName

parserCommand :: A.Parser Conf
parserCommand = ConfCommand <$> A.strOption
  (A.long "command" <> A.short 'c' <> A.metavar "COMMAND" <> A.help
    "Command to parse."
  )

parserFileName :: A.Parser Conf
parserFileName = A.argument (A.eitherReader f)
                            (A.metavar "FILE" <> A.help "Command to parse.")
 where
  f "-" = Right ConfStdin
  f s   = Right $ ConfFileName s

run :: Conf -> IO ExitCode
run (ConfCommand confCommand) = do
  let input = IS.ReplInputStrict confCommand
  IS.parseAnyStateInput @(Data.StmDb Rt.Runtime) input >>= \case
    Left err -> do
      print err
      exitFailure
    Right input' -> do
      print input'
      exitSuccess

-- TODO We need a parser for multiple commands separated by newlines.
run (ConfFileName fileName) = do
  content <- T.lines <$> readFile fileName
  print content
  exitSuccess

run ConfStdin = do
  content <- T.lines <$> getContents
  print content
  exitSuccess
