{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import qualified Control.Concurrent.Async      as Async
import qualified Options.Applicative           as A
import qualified Prototype.Example.Exe.Parse   as P
import qualified Prototype.Example.Exe.Process as P
import qualified Prototype.Example.Runtime     as Rt
import qualified Servant.Auth.Server           as Srv

--------------------------------------------------------------------------------
main :: IO ExitCode
main =
  putStrLn @Text "Parsing command-line options..."
    >>  A.execParser mainParserInfo
    >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "Prototype-hs Example program"
    <> A.progDesc
         "Interactive state demo: modify states via multiple sources of input: \
         \HTTP and a REPL."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  putStrLn @Text
    "Booting runtime; the rest of the startup logs will be in the configured logging outputs."
  jwk                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf Nothing jwk >>= either throwIO pure

  let handleExceptions = (`catch` P.shutdown runtime . Just)

  handleExceptions $ do
    (P.startServer runtime >>= P.endServer _rLoggers)
      `Async.concurrently_` (P.startRepl runtime >>= P.endRepl)
    P.shutdown runtime Nothing
