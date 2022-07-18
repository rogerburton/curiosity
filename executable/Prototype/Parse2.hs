{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Prototype.Parse2
  ( parserInfo
  , parserInfoWithTarget
  , Command(..)
  , CommandWithTarget(..)
  , CommandTarget(..)
  ) where

import qualified Commence.Runtime.Storage      as S
import qualified Options.Applicative           as A
import qualified Prototype.Data.User           as U


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
      StateFileTarget
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
-- | Describes the command available from the command-line with `cty`, or
-- within the UNIX-domain socket server, `cty-sock`.
data Command =
    Init
    -- ^ Initialise a new, empty state file.
  | State
    -- ^ Show the full state.
  | SelectUser (S.DBSelect U.UserProfile)
  | UpdateUser (S.DBUpdate U.UserProfile)
  | ShowId Text
    -- ^ If not a command per se, assume it's an ID to be looked up.
  deriving Show

-- | The same commands, defined above, can be used within the UNIX-domain
-- socket server, `cty-sock`, but also from a real command-line tool, `cty`.
-- In the later case, a user might want to direct the command-line tool to
-- interact with a server, or a local state file. This data type is meant to
-- augment the above commands with such options.
data CommandWithTarget = CommandWithTarget Command CommandTarget
  deriving Show

data CommandTarget = StateFileTarget FilePath | UnixDomainTarget FilePath
  deriving Show

parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "init"
          ( A.info (parserInit <**> A.helper)
          $ A.progDesc "Initialise a new, empty state file"
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

parserState :: A.Parser Command
parserState = pure State

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
  username <- A.argument A.str (A.metavar "USERNAME" <> A.help "A username")
  password <- A.argument A.str (A.metavar "PASSWORD" <> A.help "A password")
  email    <- A.argument A.str (A.metavar "EMAIL" <> A.help "An email address")
  return $ UpdateUser . U.UserCreate $ U.UserProfile
    "USER-0" -- TODO
    (U.Credentials username password)
    "TODO"
    email

parserDeleteUser :: A.Parser Command
parserDeleteUser = UpdateUser . U.UserDelete . U.UserId <$> A.argument
  A.str
  (A.metavar "USER-ID" <> A.help "A user ID")

parserGetUser :: A.Parser Command
parserGetUser = SelectUser . U.SelectUserById . U.UserId <$> A.argument
  A.str
  (A.metavar "USER-ID" <> A.help "A user ID")

parserShowId :: A.Parser Command
parserShowId =
  ShowId <$> A.argument A.str (A.metavar "ID" <> A.help "An object ID")
