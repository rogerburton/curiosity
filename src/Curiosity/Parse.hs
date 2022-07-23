{-# LANGUAGE ApplicativeDo #-}
module Curiosity.Parse
  ( confParser
  , replParser
  , serverParser
  ) where

import qualified Commence.InteractiveState.Repl
                                               as Repl
import qualified Commence.Multilogging         as ML
import           Control.Monad.Log             as L
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server              as Srv
import           Data.Default.Class
import qualified Options.Applicative           as A
import qualified Servant.Auth.Server           as SAuth
import qualified System.Log.FastLogger         as FL


--------------------------------------------------------------------------------
confParser :: A.Parser Rt.Conf
confParser = do
  _confDbFile <- dbFileParser
  pure Rt.Conf {
      -- FIXME: ML.parseLoggingConf never terminates, should be fixed.
                 _confLogging = Rt.defaultLoggingConf, .. }

serverParser :: A.Parser Srv.ServerConf
serverParser = do
  _serverPort <- abs <$> A.option
    A.auto
    (A.long "server-port" <> A.value 9000 <> A.metavar "PORT" <> A.help
      "Port to run the HTTP server on."
    )
  _serverStaticDir <- A.strOption
    (  A.long "static-dir"
    <> A.value "./_site/"
    <> A.metavar "DIR"
    <> A.help
         "A directory served as static assets, in particular HTML \
            \documentation."
    )
  _serverDataDir <- A.strOption
    (A.long "data-dir" <> A.value "./data/" <> A.metavar "DIR" <> A.help
      "A directory containing example data."
    )

  pure Srv.ServerConf
    {
      -- FIXME: Add support for cookie-settings parsing.
      _serverCookie        = SAuth.defaultCookieSettings
                               { SAuth.cookieIsSecure    = SAuth.NotSecure -- Use temporarily NotSecure for easier local testing with cURL.
                               , SAuth.cookieXsrfSetting = Nothing -- XSRF disabled to simplify curl calls (same as start-servant)
                               , SAuth.cookieSameSite    = SAuth.SameSiteStrict
                               }
      -- FIXME: See if this can be customized via parsing.
    , _serverMkJwtSettings = SAuth.defaultJWTSettings
    , ..
    }

replParser :: A.Parser Repl.ReplConf
replParser = do
  _replPrompt <-
    A.strOption
    $  A.long "repl-prompt"
    <> A.value (Repl._replPrompt def)
    <> A.showDefault
    <> A.metavar "PROMPT"
    <> A.help "Prompt to use for the repl"

  _replHistory <- A.switch $ A.long "repl-history-on" <> A.help
    "Flag to enable history."

  replReplExitCmds <- A.many $ A.strOption (A.long "repl-exit-cmd")

  pure Repl.ReplConf
    { _replReplExitCmds = if null replReplExitCmds
                            then ["quit"]
                            else replReplExitCmds
    , ..
    }


--------------------------------------------------------------------------------
dbFileParser :: A.Parser (Maybe FilePath)
dbFileParser =
  A.optional $ A.strOption $ A.long "db-file" <> A.help helpTxt <> A.metavar
    "FILEPATH"
 where
  helpTxt
    = "DB file to read initial DB state from. The file may be empty or may not exist, in which case(s), the file will be \
                  \created on application exit."
