{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
module Curiosity.Run
  ( run
  , interpret'
  , wordsq
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Curiosity.Process             as P
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server              as Srv
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as BS
import           Data.List                      ( last )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , send
                                                )
import qualified Options.Applicative           as A
import qualified System.Console.Haskeline      as HL
import           System.Directory               ( doesFileExist )
import           System.Environment             ( lookupEnv )
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                , takeFileName
                                                )
import           System.Posix.User              ( getLoginName )


--------------------------------------------------------------------------------
run :: Command.CommandWithTarget -> IO ExitCode
-- Convert UserFromLogin to User.
run (Command.CommandWithTarget command target Command.UserFromLogin) = do
  login <- User.UserName . T.pack <$> getLoginName
  run (Command.CommandWithTarget command target $ Command.User login)

run (Command.CommandWithTarget Command.Layout _ _) = do
  Srv.routingLayout >>= putStrLn
  exitSuccess

run (Command.CommandWithTarget Command.Init (Command.StateFileTarget path) _) =
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

run (Command.CommandWithTarget (Command.Reset conf) (Command.StateFileTarget path) _)
  = do
    runtime <-
      Rt.boot conf { P._confDbFile = Just path } >>= either throwIO pure
    Rt.reset runtime
    Rt.powerdown runtime
    exitSuccess

run (Command.CommandWithTarget (Command.Repl conf) (Command.StateFileTarget path) (Command.User user))
  = do
    runtime <- Rt.boot conf >>= either throwIO pure
    let handleExceptions = (`catch` P.shutdown runtime . Just)
    handleExceptions $ do
      repl runtime user
      P.shutdown runtime Nothing

run (Command.CommandWithTarget (Command.Serve conf serverConf) target _) = do
  -- Allow to set the static and data directories through environment
  -- variable. This is useful to bake the correct values in a Docker image
  -- or a virtual machine image, instead of having the user provide them.
  mstaticDir <- lookupEnv "CURIOSITY_STATIC_DIR"
  mdataDir   <- lookupEnv "CURIOSITY_DATA_DIR"
  let serverConf' =
        maybe identity (\a c -> c { P._serverDataDir = a }) mdataDir
          . maybe identity (\a c -> c { P._serverStaticDir = a }) mstaticDir
          $ serverConf

  case target of
    Command.MemoryTarget -> handleServe conf serverConf'
    Command.StateFileTarget path ->
      handleServe conf { P._confDbFile = Just path } serverConf'
    Command.UnixDomainTarget path -> do
      putStrLn @Text "TODO"
      exitFailure

run (Command.CommandWithTarget (Command.Run conf scriptPath) target (Command.User user))
  = case target of
    Command.MemoryTarget -> do
      handleRun P.defaultConf user scriptPath
    Command.StateFileTarget path -> do
      handleRun P.defaultConf { P._confDbFile = Just path } user scriptPath
    Command.UnixDomainTarget path -> do
      putStrLn @Text "TODO"
      exitFailure

run (Command.CommandWithTarget (Command.Parse confParser) _ _) =
  case confParser of
    Command.ConfCommand command -> do
      let result =
            A.execParserPure A.defaultPrefs Command.parserInfo
              $   T.unpack
              <$> T.words command
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

    Command.ParseObject typ fileName -> do
      content <- readFile fileName
      case typ of
        Command.ParseState -> do
          let result = Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          case result of
            Right (value :: Data.HaskDb Rt.Runtime) -> do
              print value
              exitSuccess
            Left err -> do
              print err
              exitFailure
        Command.ParseUser -> do
          let result = Aeson.eitherDecodeStrict (T.encodeUtf8 content)
          case result of
            Right (value :: User.UserProfile) -> do
              print value
              exitSuccess
            Left err -> do
              print err
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

run (Command.CommandWithTarget (Command.ViewQueue name) target (Command.User user))
  = do
    case target of
      Command.MemoryTarget -> do
        handleViewQueue P.defaultConf user name
      Command.StateFileTarget path -> do
        handleViewQueue P.defaultConf { P._confDbFile = Just path } user name
      Command.UnixDomainTarget path -> do
        putStrLn @Text "TODO"
        exitFailure

run (Command.CommandWithTarget (Command.ViewQueues queues) target (Command.User user))
  = do
    case target of
      Command.MemoryTarget -> do
        handleViewQueues P.defaultConf user queues
      Command.StateFileTarget path -> do
        handleViewQueues P.defaultConf { P._confDbFile = Just path } user queues
      Command.UnixDomainTarget path -> do
        putStrLn @Text "TODO"
        exitFailure

run (Command.CommandWithTarget Command.Step target (Command.User user)) = do
  case target of
    Command.MemoryTarget -> do
      handleCommand P.defaultConf user Command.Step
    Command.StateFileTarget path -> do
      handleCommand P.defaultConf { P._confDbFile = Just path }
                    user
                    Command.Step
    Command.UnixDomainTarget path -> do
      putStrLn @Text "TODO"
      exitFailure

run (Command.CommandWithTarget (Command.Log msg conf) (Command.StateFileTarget path) _)
  = do
    runtime <-
      Rt.boot conf { P._confDbFile = Just path } >>= either throwIO pure
    P.logInfo (<> "CLI" <> "Log") (Rt._rLoggers runtime) msg
    Rt.powerdown runtime
    exitSuccess

run (Command.CommandWithTarget (Command.ShowId i) target (Command.User user)) =
  do
    case target of
      Command.MemoryTarget -> do
        handleShowId P.defaultConf user i
      Command.StateFileTarget path -> do
        handleShowId P.defaultConf { P._confDbFile = Just path } user i
      Command.UnixDomainTarget path -> do
        putStrLn @Text "TODO"
        exitFailure

run (Command.CommandWithTarget command target (Command.User user)) = do
  case target of
    Command.MemoryTarget -> do
      handleCommand P.defaultConf user command
    Command.StateFileTarget path -> do
      handleCommand P.defaultConf { P._confDbFile = Just path } user command
    Command.UnixDomainTarget path -> do
      client path command


--------------------------------------------------------------------------------
handleServe :: P.Conf -> P.ServerConf -> IO ExitCode
handleServe conf serverConf = do
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure
  P.startServer serverConf runtime >>= P.endServer _rLoggers
  mPowerdownErrs <- Rt.powerdown runtime
  maybe exitSuccess throwIO mPowerdownErrs


--------------------------------------------------------------------------------
handleRun :: P.Conf -> User.UserName -> FilePath -> IO ExitCode
handleRun conf user scriptPath = do
  runtime <- Rt.boot conf >>= either throwIO pure
  code    <- interpret runtime user scriptPath
  Rt.powerdown runtime
  exitWith code

interpret :: Rt.Runtime -> User.UserName -> FilePath -> IO ExitCode
interpret runtime user path = do
  (_, output) <- interpret' runtime user path 0
  let len     = length $ takeWhile ((== ExitSuccess) . snd3) output
      output' = take (len + 1) output
      pad 0 = ""
      pad 1 = "> "
      pad n = T.concat (replicate ((n - 1) * 2) ">") <> "> "
  mapM_ (\(a, _, c) -> putStrLn $ pad a <> c) output'
  pure $ snd3 . last $ (0, ExitSuccess, "") : output'

snd3 (_, b, _) = b
thd3 (_, _, c) = c

interpret'
  :: Rt.Runtime
  -> User.UserName
  -> FilePath
  -> Int
  -> IO (User.UserName, [(Int, ExitCode, Text)])
interpret' runtime user path nesting = do
  content <- readFile path
  foldlM loop (user, []) $ zip [1 :: Int ..] (T.lines content)
 where
  dir = takeDirectory path
  loop (user', acc) (ln, line) = do
    let (prefix, comment) = T.breakOn "#" line
        separated         = map T.pack . wordsq $ T.unpack prefix
        grouped           = T.unwords separated
    case separated of
      []               -> pure (user', acc)
      ["as", username] -> do
        let output = [show ln <> ": " <> grouped, "Modifiying default user."]
        pure
          (User.UserName username, acc ++ map (nesting, ExitSuccess, ) output)
      ["quit"] -> do
        let output = [show ln <> ": " <> grouped, "Exiting."]
        pure (user', acc ++ map (nesting, ExitSuccess, ) output)
      input -> do
        let output_ = [show ln <> ": " <> grouped]
            result =
              A.execParserPure A.defaultPrefs Command.parserInfo
                $   T.unpack
                <$> input
        case result of
          A.Success command -> do
            case command of
              Command.Reset _ -> do
                let output = output_ ++ ["Resetting to the empty state."]
                Rt.reset runtime
                pure (user', acc ++ map (nesting, ExitSuccess, ) output)
              Command.Run _ scriptPath -> do
                (_, output') <- liftIO $ interpret' runtime
                                                    user
                                                    (dir </> scriptPath)
                                                    (succ nesting)
                pure
                  ( user'
                  , acc ++ map (nesting, ExitSuccess, ) output_ ++ output'
                  )
              _ -> do
                (_, output) <- Rt.handleCommand runtime user' command
                pure
                  ( user'
                  , acc ++ map (nesting, ExitSuccess, ) (output_ ++ output)
                  )
          A.Failure err ->
            pure (user', acc ++ [(nesting, ExitFailure 1, show err)])
          A.CompletionInvoked _ ->
            pure (user', acc ++ [(nesting, ExitFailure 1, "Shouldn't happen")])


--------------------------------------------------------------------------------
handleViewQueue conf user name = do
  case name of
    Command.EmailAddrToVerify -> do
      putStrLn @Text "Email addresses to verify:"
      handleCommand conf
                    user
                    (Command.FilterUsers User.PredicateEmailAddrToVerify)


--------------------------------------------------------------------------------
handleViewQueues conf user queues = do
  case queues of
    Command.CurrentUserQueues -> do
      -- TODO Check first if the user has the necessary rights to handle this
      -- queue.
      handleViewQueue conf user Command.EmailAddrToVerify


--------------------------------------------------------------------------------
handleShowId conf user i = do
  case T.splitOn "-" i of
    "USER" : _ -> do
      handleCommand conf user (Command.SelectUser False (User.UserId i) False)
    prefix : _ -> do
      putStrLn $ "Unknown ID prefix " <> prefix <> "."
      exitFailure
    [] -> do
      putStrLn @Text "Please provide an ID."
      exitFailure

--------------------------------------------------------------------------------
handleCommand conf user command = do
  runtime            <- Rt.boot conf >>= either handleError pure

  (exitCode, output) <- Rt.handleCommand runtime user command
  mapM_ putStrLn output

  Rt.powerdown runtime
  -- TODO shutdown runtime, loggers, save state, ...
  exitWith exitCode

handleError e
  |
    -- TODO What's the right way to pattern-match on the right type,
    -- i.e. IOErr.
    Errs.errCode e == "ERR.FILE_NOT_FOUND" = do
    putStrLn (fromMaybe "Error." $ Errs.userMessage e)
    putStrLn @Text "You may want to create a state file with `cty init`."
    exitFailure
  | otherwise = do
    putStrLn (fromMaybe "Error." $ Errs.userMessage e)
    exitFailure


--------------------------------------------------------------------------------
client :: FilePath -> Command.Command -> IO ExitCode
client path command = do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix path
  command' <- commandToString command
  send sock command'
  msg <- recv sock 1024
  let response = map B.unpack $ B.words msg -- TODO decodeUtf8
  print response
  close sock
  exitSuccess

commandToString = \case
  Command.Init -> do
    putStrLn @Text "Can't send `init` to a server."
    exitFailure
  Command.State useHs -> pure $ "state" <> if useHs then " --hs" else ""
  _                   -> do
    putStrLn @Text "Unimplemented"
    exitFailure


--------------------------------------------------------------------------------
repl :: Rt.Runtime -> User.UserName -> IO ()
repl runtime user = HL.runInputT HL.defaultSettings loop
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
            A.execParserPure A.defaultPrefs Command.parserInfo $ wordsq input
      case result of
        A.Success command -> do
          case command of
            -- We ignore the Configuration here. Probably this should be moved
            -- to Rt.handleCommand too.
            Command.Reset _          -> Rt.reset runtime
            Command.Run _ scriptPath -> do
              code <- liftIO $ interpret runtime user scriptPath
              case code of
                ExitSuccess   -> pure ()
                ExitFailure _ -> output' "Script failed."
            _ -> do
              (_, output) <- Rt.handleCommand runtime user command
              mapM_ output' output
        A.Failure           err -> output' $ show err
        A.CompletionInvoked _   -> output' "Shouldn't happen"

      loop

  prompt  = "> "

  output' = HL.outputStrLn . T.unpack


--------------------------------------------------------------------------------
-- From https://stackoverflow.com/questions/4334897/functionally-split-a-string-by-whitespace-group-by-quotes
wordsq = outside [] . (' ' :)

add c res = if null res then [[c]] else map (++ [c]) res

outside res xs = case xs of
  ' ' : ' '  : ys -> outside res $ ' ' : ys
  ' ' : '\"' : ys -> res ++ inside [] ys
  ' '        : ys -> res ++ outside [] ys
  c          : ys -> outside (add c res) ys
  _               -> res

inside res xs = case xs of
  ' '  : ' ' : ys -> inside res $ ' ' : ys
  '\"' : ' ' : ys -> res ++ outside [] (' ' : ys)
  '\"'       : [] -> res
  c          : ys -> inside (add c res) ys
  _               -> res
