-- | This is the main server-side program to interact with the server (through
-- a UNIX-domain socket) or a state file.

{-# LANGUAGE DataKinds #-}
module Main
  ( main
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , send
                                                )
import qualified Options.Applicative           as A
import qualified System.Console.Haskeline      as HL
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

run (Command.CommandWithTarget (Command.Repl conf) (Command.StateFileTarget path))
  = do
    runtime <- Rt.boot conf >>= either throwIO pure
    let handleExceptions = (`catch` P.shutdown runtime . Just)
    handleExceptions $ do
      repl runtime
      P.shutdown runtime Nothing

run (Command.CommandWithTarget (Command.Serve conf serverConf) (Command.StateFileTarget path))
  = do
    runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure
    P.startServer serverConf runtime >>= P.endServer _rLoggers
    mPowerdownErrs <- Rt.powerdown runtime
    maybe exitSuccess throwIO mPowerdownErrs

run (Command.CommandWithTarget (Command.Run conf) (Command.StateFileTarget path))
  = do
    runtime <- Rt.boot conf >>= either throwIO pure
    let handleExceptions = (`catch` P.shutdown runtime . Just)
    handleExceptions $ do
      interpret runtime "scenarios/example.txt"
      P.shutdown runtime Nothing

run (Command.CommandWithTarget (Command.Parse confParser) (Command.StateFileTarget path))
  = case confParser of
    Command.ConfCommand command -> do
      let result =
            A.execParserPure A.defaultPrefs Command.parserInfo
              . map T.unpack
              $ T.words command
      case result of
        A.Success x -> do
          print x
          exitSuccess
        A.Failure err -> do
          print err
          exitFailure
        A.CompletionInvoked _ -> do
          print @IO @Text "Shouldn't happen"
          exitFailure

    -- TODO We need a parser for multiple commands separated by newlines.
    Command.ConfFileName fileName -> do
      content <- T.lines <$> readFile fileName
      print content
      exitSuccess

    Command.ConfStdin -> do
      content <- T.lines <$> getContents
      print content
      exitSuccess

run (Command.CommandWithTarget command target) = do
  case target of
    Command.StateFileTarget path -> do
      runtime <-
        Rt.boot P.defaultConf { P._confDbFile = Just path }
          >>= either throwIO pure

      exitCode <- Rt.handleCommand runtime putStrLn command

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


--------------------------------------------------------------------------------
repl :: Rt.Runtime -> IO ()
repl runtime = HL.runInputT HL.defaultSettings loop
 where
  loop = HL.getInputLine prompt >>= \case
    Nothing     -> output' ""
    -- TODO Probably processInput below (within parseAnyStateInput) should have
    -- other possible results (beside mod and viz): comments and blanks
    -- (no-op), instead of this special empty case.
    Just ""     -> loop
    Just "quit" -> pure ()
    Just input  -> do
      let result =
            A.execParserPure A.defaultPrefs Command.parserInfo
              $ map T.unpack
              $ words
              $ T.pack input
      case result of
        A.Success command ->
          Rt.handleCommand runtime output' command >> pure ()
        A.Failure           err -> output' $ show err
        A.CompletionInvoked _   -> output' "Shouldn't happen"

      loop

  prompt  = "> "

  output' = HL.outputStrLn . T.unpack


--------------------------------------------------------------------------------
interpret :: Rt.Runtime -> FilePath -> IO ()
interpret runtime path = do
  content <- readFile path
  loop $ T.lines content
 where
  loop []            = pure ()
  loop (line : rest) = case T.words line of
    []       -> loop rest
    ["quit"] -> pure ()
    input    -> do
      let result = A.execParserPure A.defaultPrefs Command.parserInfo
            $ map T.unpack input
      case result of
        A.Success command -> do
          Rt.handleCommand runtime output' command
          loop rest
        A.Failure err -> do
          output' $ show err
          exitFailure
        A.CompletionInvoked _ -> do
          output' "Shouldn't happen"
          exitFailure

  output' = putStrLn . T.unpack
