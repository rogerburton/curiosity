-- | This is the main server-side program to interact with the server (through
-- a UNIX-domain socket) or a state file.

{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Parse               as P
import qualified Curiosity.Runtime             as Rt
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import           System.Directory               ( doesFileExist )


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser Command.parserInfoWithTarget >>= run


--------------------------------------------------------------------------------
run :: Command.CommandWithTarget -> IO ExitCode
run (Command.CommandWithTarget Command.Init (Command.StateFileTarget path)) = do
  exists <- liftIO $ doesFileExist path
  if exists
    then do
      putStrLn @Text $ "The file '" <> T.pack path <> "' already exists."
      putStrLn @Text "Aborting."
      exitFailure
    else do
      let bs = Data.serialiseDb Data.emptyHask
      try @SomeException (BS.writeFile path bs) >>= either
        (\e -> print e >> exitFailure)
        (const $ do
          putStrLn @Text $ "State file '" <> T.pack path <> "' created."
          exitSuccess
        )

run (Command.CommandWithTarget command target) = do
  case target of
    Command.StateFileTarget path -> do
      runtime@Rt.Runtime {..} <-
        Rt.boot P.defaultConf { Rt._confDbFile = Just path }
          >>= either throwIO pure

      exitCode <- Command.handleCommand runtime putStrLn command

      Rt.powerdown runtime
      -- TODO shutdown runtime, loggers, save state, ...
      exitWith exitCode

    Command.UnixDomainTarget _ -> do
      putStrLn @Text "Unimplemented: --socket, a.k.a UnixDomainTarget"
      exitFailure
