module Main
  ( main
  ) where

import qualified Commence.InteractiveState.Repl
                                               as Repl
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo (Rt.Conf, Repl.ReplConf)
mainParserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-repl - Curiosity REPL"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."
  where parser = (,) <$> P.confParser <*> P.replParser

runWithConf :: (Rt.Conf, Repl.ReplConf) -> IO ExitCode
runWithConf (conf, replConf) = do
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)

  handleExceptions $ do
    P.startRepl replConf runtime >>= P.endRepl
    P.shutdown runtime Nothing
