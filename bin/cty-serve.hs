{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server              as Srv
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Srv.ServerConf
mainParserInfo =
  A.info (P.serverParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-serve - Curiosity HTTP server"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Srv.ServerConf -> IO ExitCode
runWithConf conf = do
  -- TODO Parse the runtime config.
  runtime@Rt.Runtime {..} <- Rt.boot P.defaultConf >>= either throwIO pure

  P.startServer conf runtime >>= P.endServer _rLoggers

  mPowerdownErrs <- Rt.powerdown runtime

  maybe exitSuccess throwIO mPowerdownErrs
