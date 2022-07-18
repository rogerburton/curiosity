{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Options.Applicative           as A
import qualified Prototype.Parse               as P
import qualified Prototype.Process             as P
import qualified Prototype.Runtime             as Rt
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "Curiosity"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  jwk                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf jwk >>= either throwIO pure

  P.startServer runtime >>= P.endServer _rLoggers

  mPowerdownErrs <- Rt.powerdown runtime

  maybe exitSuccess throwIO mPowerdownErrs
