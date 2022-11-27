-- | This is a simple UNIX-domain socket REPL server. The accepted commands
-- re-use the optparse-applicative parsers behind the `cty` command-line tool,
-- ensuring a similar experience. It is possible to interact with this server
-- with e.g.:
--
--   nc -U curiosity.sock

import qualified Curiosity.Parse               as P
import qualified Curiosity.Runtime             as Rt
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo P.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-sock - Curiosity's UNIX-domain server"
    <> A.progDesc "TODO"

runWithConf conf = do
  putStrLn @Text "Creating runtime..."
  runtime <- Rt.bootConf conf Rt.NoThreads >>= either throwIO pure
  Rt.runWithRuntime runtime
