{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-serve - Curiosity HTTP server"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure

  P.startServer runtime >>= P.endServer _rLoggers

  mPowerdownErrs <- Rt.powerdown runtime

  maybe exitSuccess throwIO mPowerdownErrs
