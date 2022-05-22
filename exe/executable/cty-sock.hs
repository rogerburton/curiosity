-- | This is a simple UNIX-domain socket REPL server. The accepted commands
-- re-use the optparse-applicative parsers behind the `cty` command-line tool,
-- ensuring a similar experience. It is possible to interact with this server
-- with e.g.:
--
--   nc -U curiosity.sock

import qualified Data.ByteString.Char8         as B
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Exe.Data        as Data
import qualified Prototype.Exe.Data.User   as User
import qualified Prototype.Exe.Exe.Parse   as P
import qualified Prototype.Exe.Exe.Parse2  as P
import qualified Prototype.Exe.Runtime     as Rt
import qualified Servant.Auth.Server           as Srv


--------------------------------------------------------------------------------
main :: IO ()
main = do
  A.execParser mainParserInfo
  >>= runWithConf

mainParserInfo :: A.ParserInfo Rt.Conf
mainParserInfo =
  A.info (P.confParser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-sock - Curiosity's UNIX-domain server"
    <> A.progDesc "TODO"

runWithConf conf = do
  putStrLn @Text "Creating runtime..."
  jwt                     <- Srv.generateKey
  runtime@Rt.Runtime {..} <- Rt.boot conf Nothing jwt >>= either throwIO pure
  -- TODO jwt should'nt be in the runtime, but in the HTTP layer

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
    ["quit"] -> return ()
    [] -> repl runtime conn
    _ -> do
      let result = A.execParserPure A.defaultPrefs P.parserInfo command
      case result of
        A.Success x -> print x >> sendAll conn (show x <> "\n")
        A.Failure err -> print err
        A.CompletionInvoked _ -> print "Shouldn't happen"

      output <- Rt.runExeAppMSafe runtime $
        IS.execModification (Data.ModifyUser (User.UserCreate $ User.UserProfile (User.UserCreds "alice" "pass") "Alice"))

      print output
        
      repl runtime conn
