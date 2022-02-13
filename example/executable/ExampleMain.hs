{-# LANGUAGE DeriveAnyClass #-}
module ExampleMain
  ( main
  ) where

import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Exe.Parse   as P
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Runtime.Errors      as Errs

main :: IO ExitCode
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info P.confParser
    $  A.fullDesc
    <> A.header "Prototype-hs Example program"
    <> A.progDesc
         "Interactive state demo: modify states via multiple sources of input: HTTP and a REPL."

runWithConf :: Rt.Conf -> IO ExitCode
runWithConf conf = do
  -- The first step is to boot up a runtime. 
  runtime <- Rt.boot conf Nothing >>= either throwIO pure
  undefined

