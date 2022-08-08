{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Curiosity.Command
  ( Command(..)
  , ParseConf(..)
  , CommandWithTarget(..)
  , CommandTarget(..)
  , ObjectType(..)
  , parserInfo
  , parserInfoWithTarget
  ) where

import qualified Commence.Runtime.Storage      as S
import qualified Curiosity.Data.User           as U
import qualified Curiosity.Parse               as P
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
-- | Describes the command available from the command-line with `cty`, or
-- within the UNIX-domain socket server, `cty-sock`, or the `cty-repl-2` REPL.
data Command =
    Init
    -- ^ Initialise a new, empty state file.
  | Repl P.Conf
    -- ^ Run a REPL.
  | Serve P.Conf P.ServerConf
    -- ^ Run an HTTP server.
  | Run P.Conf FilePath
    -- ^ Interpret a script.
  | Parse ParseConf
    -- ^ Parse a single command.
  | State Bool
    -- ^ Show the full state. If True, use Haskell format instead of JSON.
  | CreateUser U.Signup
  | SelectUser Bool U.UserId
    -- ^ Show a give user. If True, use Haskell format instead of JSON.
  | UpdateUser (S.DBUpdate U.UserProfile)
  | ShowId Text
    -- ^ If not a command per se, assume it's an ID to be looked up.
  deriving Show

data ParseConf =
    ConfCommand Text
  | ConfFileName FilePath
  | ConfStdin
  | ParseObject ObjectType FilePath
  deriving Show

data ObjectType = ParseState | ParseUser
  deriving Show

-- | The same commands, defined above, can be used within the UNIX-domain
-- socket server, `cty-sock`, but also from a real command-line tool, `cty`.
-- In the later case, a user might want to direct the command-line tool to
-- interact with a server, or a local state file. This data type is meant to
-- augment the above commands with such options.
data CommandWithTarget = CommandWithTarget Command CommandTarget
  deriving Show

data CommandTarget = MemoryTarget | StateFileTarget FilePath | UnixDomainTarget FilePath
  deriving Show


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty-sock - Curiosity's UNIX-domain socket server"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
        \of a web application for Smart.\n\n\
        \cty-sock offers a networked REPL exposed over a UNIX-domain socket."

parserInfoWithTarget :: A.ParserInfo CommandWithTarget
parserInfoWithTarget =
  A.info (parser' <**> A.helper)
    $  A.fullDesc
    <> A.header "cty - Curiosity's main server-side program"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
        \of a web application for Smart.\n\n\
        \cty offers a command-line interface against a running server or \
        \a state file."
 where
  parser' = do
    target <-
      (A.flag' MemoryTarget $ A.short 'm' <> A.long "memory" <> A.help
        "Don't use a state file or a UNIX-domain socket."
      )
      <|> StateFileTarget
      <$> (  A.strOption
          $  A.short 's'
          <> A.long "state"
          <> A.value "state.json"
          <> A.help "A state file. Default is 'state.json'."
          <> A.metavar "FILEPATH"
          )
      <|> UnixDomainTarget
      <$> (  A.strOption
          $  A.short 't'
          <> A.long "socket"
          <> A.help "A UNIX-domain socket"
          <> A.metavar "FILEPATH"
          )
    command <- parser
    return $ CommandWithTarget command target


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "init"
          ( A.info (parserInit <**> A.helper)
          $ A.progDesc "Initialise a new, empty state file"
          )

      <> A.command
           "repl"
           (A.info (parserRepl <**> A.helper) $ A.progDesc "Start a REPL")

      <> A.command
           "serve"
           ( A.info (parserServe <**> A.helper)
           $ A.progDesc "Run the Curiosity HTTP server"
           )

      <> A.command
           "run"
           (A.info (parserRun <**> A.helper) $ A.progDesc "Interpret a script"
           )

      <> A.command
           "parse"
           ( A.info (parserParse <**> A.helper)
           $ A.progDesc "Parse a single command"
           )

      <> A.command
           "state"
           ( A.info (parserState <**> A.helper)
           $ A.progDesc "Show the full state"
           )

      <> A.command
           "user"
           ( A.info (parserUser <**> A.helper)
           $ A.progDesc "User-related commands"
           )
      )
    <|> parserShowId

parserInit :: A.Parser Command
parserInit = pure Init

parserRepl :: A.Parser Command
parserRepl = Repl <$> P.confParser

parserServe :: A.Parser Command
parserServe = Serve <$> P.confParser <*> P.serverParser

parserRun :: A.Parser Command
parserRun = Run <$> P.confParser <*> A.argument
  A.str
  (A.metavar "FILE" <> A.help "Script to run.")

parserParse :: A.Parser Command
parserParse = Parse <$> (parserCommand <|> parserObject <|> parserFileName)

parserCommand :: A.Parser ParseConf
parserCommand = ConfCommand <$> A.strOption
  (A.long "command" <> A.short 'c' <> A.metavar "COMMAND" <> A.help
    "Command to parse."
  )

parserFileName :: A.Parser ParseConf
parserFileName = A.argument (A.eitherReader f)
                            (A.metavar "FILE" <> A.help "Command to parse.")
 where
  f "-" = Right ConfStdin
  f s   = Right $ ConfFileName s

parserObject :: A.Parser ParseConf
parserObject =
  ParseObject
    <$> A.option
          (A.eitherReader f)
          (A.long "object" <> A.metavar "TYPE" <> A.help
            "Type of the object to parse."
          )
    <*> A.argument A.str (A.metavar "FILE" <> A.help "Object to parse.")
 where
  f "state" = Right ParseState
  f "user"  = Right ParseUser
  f _       = Left "Unrecognized object type."

parserState :: A.Parser Command
parserState = State <$> A.switch
  (A.long "hs" <> A.help "Use the Haskell format (default is JSON).")

parserUser :: A.Parser Command
parserUser = A.subparser
  (  A.command
      "create"
      (A.info (parserCreateUser <**> A.helper) $ A.progDesc "Create a new user")
  <> A.command
       "delete"
       (A.info (parserDeleteUser <**> A.helper) $ A.progDesc "Delete a user")
  <> A.command
       "get"
       (A.info (parserGetUser <**> A.helper) $ A.progDesc "Select a user")
  )

parserCreateUser :: A.Parser Command
parserCreateUser = do
  username   <- A.argument A.str (A.metavar "USERNAME" <> A.help "A username")
  password   <- A.argument A.str (A.metavar "PASSWORD" <> A.help "A password")
  email <- A.argument A.str (A.metavar "EMAIL" <> A.help "An email address")
  tosConsent <- A.switch
    (  A.long "accept-tos"
    <> A.help "Indicate if the user being created consents to the TOS."
    )
  return $ CreateUser $ U.Signup username password email tosConsent

parserDeleteUser :: A.Parser Command
parserDeleteUser = UpdateUser . U.UserDelete . U.UserId <$> A.argument
  A.str
  (A.metavar "USER-ID" <> A.help "A user ID")

parserGetUser :: A.Parser Command
parserGetUser =
  SelectUser
    <$> A.switch
          (A.long "hs" <> A.help "Use the Haskell format (default is JSON).")
    <*> A.argument A.str (A.metavar "USER-ID" <> A.help "A user ID")

parserShowId :: A.Parser Command
parserShowId =
  ShowId <$> A.argument A.str (A.metavar "ID" <> A.help "An object ID")
