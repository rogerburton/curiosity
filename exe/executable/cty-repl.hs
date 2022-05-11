module Main
  ( main
  ) where

import qualified Options.Applicative           as A
import qualified Prototype.Exe.Exe.Parse   as P
import qualified Prototype.Exe.Exe.Process as P
import qualified Prototype.Exe.Runtime     as Rt
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "Curiosity REPL"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  jwk                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf Nothing jwk >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)

  handleExceptions $ do
    (P.startRepl runtime >>= P.endRepl)
    P.shutdown runtime Nothing
