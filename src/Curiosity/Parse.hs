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
  , defaultServerConf
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

instance Eq Conf where
  -- TODO This should be automatically derived: see to fix ML.LoggingConf.
  a == b = _confDbFile a == _confDbFile b

instance Show Conf

makeLenses ''Conf

-- | HTTP server config.
data ServerConf = ServerConf
  { _serverPort          :: Int
  , _serverStaticDir     :: FilePath
  , _serverDataDir       :: FilePath
  , _serverScenariosDir  :: FilePath
  , _serverCookie        :: SAuth.CookieSettings
    -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _serverMkJwtSettings :: JWK.JWK -> SAuth.JWTSettings
    -- ^ JWK settings to use, depending on the key employed.
  }

instance Eq ServerConf where
  -- TODO This should be automatically derived: see to fix _serverMkJwtSettings.
  a == b =
    _serverPort b
      == _serverPort b
      && _serverStaticDir a
      == _serverStaticDir b
      && _serverDataDir a
      == _serverDataDir b
      && _serverScenariosDir a
      == _serverScenariosDir b
      && _serverCookie a
      == _serverCookie b


instance Show ServerConf

--------------------------------------------------------------------------------
defaultConf :: Conf
defaultConf =
  let _confDbFile = Nothing in Conf { _confLogging = defaultLoggingConf, .. }

defaultLoggingConf :: ML.LoggingConf
defaultLoggingConf = mkLoggingConf "./curiosity.log"

mkLoggingConf :: FilePath -> ML.LoggingConf
mkLoggingConf path = ML.LoggingConf [FL.LogFile (flspec path) 1024]
                                    "Curiosity"
                                    L.levelInfo

flspec :: FilePath -> FL.FileLogSpec
flspec path = FL.FileLogSpec path (1024 * 1024) 10
  -- 1MB, or about 5240 200-character lines, and keeping 10 rotated files.


--------------------------------------------------------------------------------
confParser :: A.Parser Conf
confParser = do
  _confDbFile  <- dbFileParser
  _confLogFile <- A.strOption
    (A.long "log" <> A.value "./curiosity.log" <> A.metavar "PATH" <> A.help
      "A file where to write logs."
    )
  pure Conf {
      -- FIXME: ML.parseLoggingConf never terminates, should be fixed.
              _confLogging = mkLoggingConf _confLogFile, .. }

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
  _serverScenariosDir <- A.strOption
    (A.long "scenarios-dir" <> A.value "./scenarios/" <> A.metavar "DIR" <> A.help
      "A directory containing scenarios."
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

defaultServerConf :: ServerConf
defaultServerConf = ServerConf
  { _serverCookie        = SAuth.defaultCookieSettings
                             { SAuth.cookieIsSecure    = SAuth.NotSecure
                             , SAuth.cookieXsrfSetting = Nothing
                             , SAuth.cookieSameSite    = SAuth.SameSiteStrict
                             }
  , _serverMkJwtSettings = SAuth.defaultJWTSettings
  , _serverPort          = 9000
  , _serverStaticDir     = "./_site/"
  , _serverDataDir       = "./data/"
  , _serverScenariosDir  = "./scenarios/"
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
