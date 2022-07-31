{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
module Curiosity.Parse
  ( Conf(..)
  , confLogging
  , confDbFile
  , ServerConf(..)
  , defaultConf
  , defaultLoggingConf
  , mkLoggingConf
  , confParser
  , serverParser
  ) where

import qualified Commence.Multilogging         as ML
import           Control.Lens                  as Lens
import qualified Control.Monad.Log             as L
import qualified Crypto.JOSE.JWK               as JWK
import qualified Options.Applicative           as A
import qualified Servant.Auth.Server           as SAuth
import qualified System.Log.FastLogger         as FL


--------------------------------------------------------------------------------
-- | Application config.
data Conf = Conf
  { _confLogging :: ML.LoggingConf -- ^ Logging configuration.
  , _confDbFile  :: Maybe FilePath
    -- ^ An optional filepath to write the DB to, or read it from. If the file
    -- is absent, it will be created on server exit, with the latest DB state
    -- written to it.
  }

instance Show Conf

makeLenses ''Conf

-- | HTTP server config.
data ServerConf = ServerConf
  { _serverPort          :: Int
  , _serverStaticDir     :: FilePath
  , _serverDataDir       :: FilePath
  , _serverCookie        :: SAuth.CookieSettings
    -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _serverMkJwtSettings :: JWK.JWK -> SAuth.JWTSettings
    -- ^ JWK settings to use, depending on the key employed.
  }

instance Show ServerConf

--------------------------------------------------------------------------------
defaultConf :: Conf
defaultConf =
  let _confDbFile = Nothing in Conf { _confLogging = defaultLoggingConf, .. }

defaultLoggingConf :: ML.LoggingConf
defaultLoggingConf = mkLoggingConf "/tmp/curiosity.log"

mkLoggingConf :: FilePath -> ML.LoggingConf
mkLoggingConf path =
  ML.LoggingConf [FL.LogFile (flspec path) 1024] "Curiosity" L.levelInfo

flspec :: FilePath -> FL.FileLogSpec
flspec path = FL.FileLogSpec path 5000 0


--------------------------------------------------------------------------------
confParser :: A.Parser Conf
confParser = do
  _confDbFile <- dbFileParser
  pure Conf {
      -- FIXME: ML.parseLoggingConf never terminates, should be fixed.
                 _confLogging = defaultLoggingConf, .. }

serverParser :: A.Parser ServerConf
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

  pure ServerConf
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


--------------------------------------------------------------------------------
dbFileParser :: A.Parser (Maybe FilePath)
dbFileParser =
  A.optional $ A.strOption $ A.long "db-file" <> A.help helpTxt <> A.metavar
    "FILEPATH"
 where
  helpTxt
    = "DB file to read initial DB state from. The file may be empty or may not exist, in which case(s), the file will be \
                  \created on application exit."
