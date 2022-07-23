-- | This is the main server-side program to interact with the server (through
-- a UNIX-domain socket) or a state file.

{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Runtime             as Rt
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import           Network.Socket          hiding ( recv
                                                , send
                                                )
import           Network.Socket.ByteString      ( recv
                                                , send
                                                )
import qualified Options.Applicative           as A
import           System.Directory               ( doesFileExist )


--------------------------------------------------------------------------------
main :: IO ExitCode
main = A.execParser Command.parserInfoWithTarget >>= run


--------------------------------------------------------------------------------
run :: Command.CommandWithTarget -> IO ExitCode
run (Command.CommandWithTarget Command.Init (Command.StateFileTarget path)) =
  do
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
        Rt.boot Rt.defaultConf { Rt._confDbFile = Just path }
          >>= either throwIO pure

      exitCode <- Command.handleCommand runtime putStrLn command

      Rt.powerdown runtime
      -- TODO shutdown runtime, loggers, save state, ...
      exitWith exitCode

    Command.UnixDomainTarget path -> do
      client path command

client :: FilePath -> Command.Command -> IO ExitCode
client path command = do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix path
  let command' = commandToString command
  send sock command'
  msg <- recv sock 1024
  let response = map B.unpack $ B.words msg -- TODO decodeUtf8
  print response
  close sock
  exitSuccess

commandToString = \case
  Command.Init  -> undefined -- error "Can't send `init` to a server."
  Command.State -> "state"
  _             -> undefined -- error "Unimplemented"
