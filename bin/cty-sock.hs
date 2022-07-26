-- | This is a simple UNIX-domain socket REPL server. The accepted commands
-- re-use the optparse-applicative parsers behind the `cty` command-line tool,
-- ensuring a similar experience. It is possible to interact with this server
-- with e.g.:
--
--   nc -U curiosity.sock

import qualified Commence.InteractiveState.Class
                                               as IS
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Curiosity.Parse2              as P
import qualified Curiosity.Runtime             as Rt
import qualified Data.ByteString.Char8         as B
import           Network.Socket          hiding ( recv )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser mainParserInfo >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-sock - Curiosity's UNIX-domain server"
    <> A.progDesc "TODO"

runWithConf conf = do
  putStrLn @Text "Creating runtime..."
  runtime@Rt.Runtime {..} <- Rt.boot conf >>= either throwIO pure

  putStrLn @Text "Creating curiosity.sock..."
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix "curiosity.sock"
  listen sock maxListenQueue

  putStrLn @Text "Listening on curiosity.sock..."
  server runtime sock -- TODO bracket (or catch) and close
  close sock

server runtime sock = do
  (conn, _) <- accept sock -- TODO bracket (or catch) and close too
  void $ forkFinally
    (handler runtime conn)
    (const $ putStrLn @Text "Closing connection." >> close conn)
  server runtime sock

handler runtime conn = do
  putStrLn @Text "New connection..."
  sendAll conn "Curiosity UNIX-domain socket server.\n"
  repl runtime conn

repl runtime conn = do
  msg <- recv conn 1024
  let command = map B.unpack $ B.words msg -- TODO decodeUtf8
  case command of
    _ | B.null msg -> return () -- Connection lost.
    ["quit"]       -> return ()
    []             -> repl runtime conn
    _              -> do
      let result = A.execParserPure A.defaultPrefs P.parserInfo command
      case result of
        A.Success           x   -> print x >> sendAll conn (show x <> "\n")
        A.Failure           err -> print err
        A.CompletionInvoked _   -> print @IO @Text "Shouldn't happen"

      output <- Rt.runAppMSafe runtime $ IS.execModification
        (Data.ModifyUser
          (User.UserCreate $ User.UserProfile
            "USER-0"
            (User.Credentials "alice" "pass")
            "Alice"
            "alice@example.com"
          )
        )

      print output

      repl runtime conn
