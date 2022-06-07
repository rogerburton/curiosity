{-# LANGUAGE ApplicativeDo #-}
module Prototype.Exe.Exe.Parse
  ( confParser
  , defaultConf
  ) where

import           Control.Monad.Log             as L
import           Data.Default.Class
import qualified MultiLogging                  as ML
import qualified Options.Applicative           as A
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import           Prototype.Exe.Runtime
import qualified Servant.Auth.Server           as Srv
import qualified System.Log.FastLogger         as FL


--------------------------------------------------------------------------------
confParser :: A.Parser Conf
confParser = do
  _confServer <- serverParser
  _confRepl   <- replParser
  _confDbFile <- dbFileParser
  pure Conf
    {
      -- FIXME: ML.parseLoggingConf never terminates, should be fixed.
      _confLogging       = ML.LoggingConf [FL.LogFile flspec 1024]
                                          "PrototypeExe"
                                          L.levelInfo-- ML.parseLoggingConf
      -- FIXME: Add support for cookie-settings parsing.
    , _confCookie        = Srv.defaultCookieSettings
                             { Srv.cookieIsSecure    = Srv.NotSecure -- Use temporarily NotSecure for easier local testing with cURL.
                             , Srv.cookieXsrfSetting = Nothing -- XSRF disabled to simplify curl calls (same as start-servant)
                             , Srv.cookieSameSite    = Srv.SameSiteStrict
                             }
      -- FIXME: See if this can be customized via parsing.
    , _confMkJwtSettings = Srv.defaultJWTSettings
    , ..
    }
  where flspec = FL.FileLogSpec "/tmp/prototype-hs.log" 5000 0

defaultConf :: Conf
defaultConf =
  let _confServer = ServerConf 9000
      _confRepl   = Repl.ReplConf "> " False ["exit", "quit"]
      _confDbFile = Nothing
      flspec      = FL.FileLogSpec "/tmp/prototype-hs.log" 5000 0
  in  Conf
        { _confLogging       = ML.LoggingConf [FL.LogFile flspec 1024]
                                              "PrototypeExe"
                                              L.levelInfo
        , _confCookie        = Srv.defaultCookieSettings
                                 { Srv.cookieIsSecure    = Srv.NotSecure
                                 , Srv.cookieXsrfSetting = Nothing
                                 , Srv.cookieSameSite    = Srv.SameSiteStrict
                                 }
        , _confMkJwtSettings = Srv.defaultJWTSettings
        , ..
        }

serverParser :: A.Parser ServerConf
serverParser = ServerConf . abs <$> A.option
  A.auto
  (A.long "server-port" <> A.value 9000 <> A.metavar "PORT" <> A.help
    "Port to run the HTTP server on."
  )

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

dbFileParser :: A.Parser (Maybe FilePath)
dbFileParser =
  A.optional $ A.strOption $ A.long "db-file" <> A.help helpTxt <> A.metavar
    "FILEPATH"
 where
  helpTxt
    = "DB file to read initial DB state from. The file may be empty or may not exist, in which case(s), the file will be \
                  \created on application exit."
