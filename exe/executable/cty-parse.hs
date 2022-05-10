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

import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Exe.Data        as Data
import qualified Prototype.Exe.Runtime     as Rt


--------------------------------------------------------------------------------
main :: IO ExitCode
main = do
  let input = IS.ReplInputStrict "viz all"
  IS.parseAnyStateInput @(Data.StmDb Rt.Runtime) input >>= \case
    Left  err    -> do
      print err
      exitFailure
    Right input' -> do
      print input'
      exitSuccess
