{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Prototype.Exe.Exe.Parse2
  ( parserInfo
  , Command(..)
  ) where

import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "cty - Curiosity's main server-side program"
    <> A.progDesc
         "Curiosity is a prototype application to explore the design space \
         \of a web application for Smart.\n\n\
         \cty offers a command-line interface against a running server or \
         \a state file."


--------------------------------------------------------------------------------
data Command =
    CreateUser Text Text Text
    -- ^ Username, email address, and password
  | DeleteUser Text
    -- ^ User ID.
  | ShowId Text
    -- ^ If not a command per se, assume it's an ID to be looked up.
  deriving Show

parser :: A.Parser Command
parser =
  (A.subparser $
       A.command "create-user" (A.info (parserCreateUser <**> A.helper)
         $ A.progDesc "Create a new user")
    <> A.command "delete-user" (A.info (parserDeleteUser <**> A.helper)
         $ A.progDesc "Delete a user")
  )
  <|> parserShowId

parserCreateUser :: A.Parser Command
parserCreateUser = CreateUser
  <$> A.argument A.str
    (A.metavar "USERNAME" <> A.help
      "A username"
    )
  <*> A.argument A.str
    (A.metavar "EMAIL" <> A.help
      "An email address"
    )
  <*> A.argument A.str
    (A.metavar "PASSWORD" <> A.help
      "A password"
    )

parserDeleteUser :: A.Parser Command
parserDeleteUser = DeleteUser
  <$> A.argument A.str
    (A.metavar "USER-ID" <> A.help
      "A user ID"
    )

parserShowId :: A.Parser Command
parserShowId = ShowId <$> A.argument A.str
  (A.metavar "ID" <> A.help
    "An object ID"
  )
