-- | This is the main server-side program to interact with the server.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
  ( main
  ) where

import qualified Options.Applicative           as A
import qualified Prototype.Exe.Exe.Parse2  as P


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser P.parserInfo >>= run


--------------------------------------------------------------------------------
run :: P.Command -> IO ExitCode
run cmd = do
  print cmd
  exitSuccess
