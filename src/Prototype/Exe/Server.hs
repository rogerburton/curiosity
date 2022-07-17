{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{- |
Module: Prototype.Exe.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Prototype.Exe.Server
  (
    -- * Top level server types.
    Exe
  , exampleT
  , exampleApplication
  , runExeServer
  -- * Type-aliases for convenience
  , ServerSettings
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Control.Lens
import qualified Data.ByteString.Lazy          as BL
                                                ( hGetContents )
import           Data.List                      ( init
                                                , last
                                                )
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Prototype.Exe.Data.User       as User
import qualified Prototype.Exe.Form.Login      as Login
import qualified Prototype.Exe.Form.Signup     as Signup
import qualified Prototype.Exe.Runtime         as Rt
import qualified Prototype.Exe.Server.Private  as Priv
import qualified Prototype.Exe.Server.Private.Auth
                                               as Auth
import qualified Prototype.Exe.Server.Private.Pages
                                               as Pages
import qualified Prototype.Exe.Server.Public   as Pub
import qualified Prototype.Exe.Server.Public.Pages
                                               as Pages
import           Servant
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Servant.Server                as Server
import           Smart.Server.Page              ( PageEither )
import qualified Smart.Server.Page             as SS.P
import           System.FilePath                ( (</>) )
import           System.PosixCompat.Files       ( fileSize
                                                , getFileStatus
                                                , isRegularFile
                                                , modificationTime
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )
import           WaiAppStatic.Storage.Filesystem
                                                ( ETagLookup
                                                , defaultWebAppSettings
                                                )
import           WaiAppStatic.Types             ( File(..)
                                                , LookupResult(..)
                                                , Piece(..)
                                                , Pieces
                                                , ss404Handler
                                                , ssLookupFile
                                                , toPiece
                                                )

import           System.IO                      ( IOMode(..)
                                                , withBinaryFile
                                                )

import           Crypto.Hash                    ( Digest
                                                , MD5
                                                , hashlazy
                                                )
import           Data.ByteArray.Encoding


--------------------------------------------------------------------------------
type ServerSettings = '[SAuth.CookieSettings , SAuth.JWTSettings]

-- brittany-disable-next-binding 
type Exe = Auth.UserAuthentication :> Get '[B.HTML] (PageEither
               Pages.LandingPage
               Pages.WelcomePage
             )
             :<|> "forms" :> "login" :> Get '[B.HTML] Login.Page
             :<|> "forms" :> "signup" :> Get '[B.HTML] Signup.Page

             :<|> "echo" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.Credentials
                  :> Post '[B.HTML] Login.ResultPage
             :<|> "echo" :> "signup"
                  :> ReqBody '[FormUrlEncoded] User.Signup
                  :> Post '[B.HTML] Signup.ResultPage

             :<|> "login" :> Get '[B.HTML] Login.Page
             :<|> "signup" :> Get '[B.HTML] Signup.Page

             :<|> Public
             :<|> "private" :> Priv.Private
             :<|> Raw -- Catchall for static files (documentation)
                      -- and for a custom 404

exampleT :: forall m . Pub.PublicServerC m => FilePath -> ServerT Exe m
exampleT root =
  showLandingPage
    :<|> documentLoginPage
    :<|> documentSignupPage
    :<|> echoLogin
    :<|> echoSignup
    :<|> showLoginPage
    :<|> showSignupPage
    :<|> publicT
    :<|> Priv.privateT
    :<|> serveDocumentation root

-- | Run as a Wai Application
exampleApplication
  :: forall m
   . Pub.PublicServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an arbitrary @m@ to a Servant @Handler@
  -> Server.Context ServerSettings
  -> FilePath
  -> Wai.Application
exampleApplication handlerNatTrans ctx root =
  Servant.serveWithContext exampleProxy ctx
    $ hoistServerWithContext exampleProxy settingsProxy handlerNatTrans
    $ exampleT root
 where
  exampleProxy  = Proxy @Exe
  settingsProxy = Proxy @ServerSettings

runExeServer
  :: forall m
   . MonadIO m
  => Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
runExeServer runtime@Rt.Runtime {..} = liftIO $ Warp.run port waiApp
 where
  Rt.ServerConf port root = runtime ^. Rt.rConf . Rt.confServer
  waiApp                  = exampleApplication @Rt.ExeAppM
    (Rt.exampleAppMHandlerNatTrans runtime)
    ctx
    root
  ctx =
    _rConf
      ^.        Rt.confCookie
      Server.:. _rJwtSettings
      Server.:. Server.EmptyContext


--------------------------------------------------------------------------------
-- | Show the landing page when the user is not logged in, or the welcome page
-- when the user is logged in.
showLandingPage
  :: Pub.PublicServerC m
  => SAuth.AuthResult User.UserId
  -> m (PageEither
           -- We don't use SS.P.Public Void, nor SS.P.Public 'Authd UserProfile
           -- to not have the automatic heading.
                   Pages.LandingPage Pages.WelcomePage)
showLandingPage = \case
  SAuth.Authenticated userId ->
    S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
      Nothing -> do
        ML.warning
          "Cookie-based authentication succeeded, but the user ID is not found."
        authFailedErr $ "No user found with ID " <> show userId
      Just userProfile -> pure $ SS.P.PageR Pages.WelcomePage
  _ -> pure $ SS.P.PageL Pages.LandingPage
  where authFailedErr = Errs.throwError' . User.UserNotFound


--------------------------------------------------------------------------------
showSignupPage :: Pub.PublicServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: Pub.PublicServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

echoSignup :: Pub.PublicServerC m => User.Signup -> m Signup.ResultPage
echoSignup input = pure $ Signup.Success $ show input


--------------------------------------------------------------------------------
showLoginPage :: Pub.PublicServerC m => m Login.Page
showLoginPage = pure $ Login.Page "/a/login"

documentLoginPage :: Pub.PublicServerC m => m Login.Page
documentLoginPage = pure $ Login.Page "/echo/login"

echoLogin :: Pub.PublicServerC m => User.Credentials -> m Login.ResultPage
echoLogin input = pure $ Login.Success $ show input


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | A publicly available login page.
type Public = "a" :> "signup"
                 :> ReqBody '[FormUrlEncoded] User.Signup
                 :> Post '[B.HTML] (SS.P.Page 'SS.P.Public Void Pages.SignupResultPage)
            :<|>  "a" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.Credentials
                  :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                              NoContent
                                            )

publicT :: forall m . Pub.PublicServerC m => ServerT Public m
publicT = handleSignup :<|> handleLogin

handleSignup User.Signup {..} = env $ do
  ML.info $ "Signing up new user: " <> show username <> "..."
  ids <- S.dbUpdate $ User.UserCreateGeneratingUserId username password email
  case headMay ids of
    Just uid -> do
      ML.info $ "User created: " <> show uid <> ". Sending success result."
      pure . SS.P.PublicPage $ Pages.SignupSuccess uid
    -- TODO Failure to create a user re-using an existing username doesn't
    -- trigger the Nothing case.
    Nothing -> do
      -- TODO This should not be a 200 OK result.
      ML.info $ "Failed to create a user. Sending failure result."
      pure . SS.P.PublicPage $ Pages.SignupFailed "Failed to create users."
  where env = ML.localEnv (<> "HTTP" <> "Signup")

handleLogin User.Credentials {..} =
  env $ findMatchingUsers <&> headMay >>= \case
    Just u -> do
      ML.info "Found user, generating authentication cookies..."
      jwtSettings   <- asks Rt._rJwtSettings
      Rt.Conf {..}  <- asks Rt._rConf
      mApplyCookies <- liftIO
        $ SAuth.acceptLogin _confCookie jwtSettings (u ^. User.userProfileId)
      ML.info "Applying cookies..."
      case mApplyCookies of
        Nothing -> do
          -- TODO What can cause a failure here ?
          ML.warning "Applying cookies failed. Sending failure result."
          unauthdErr _userCredsName
        Just applyCookies -> do
          ML.info "Cookies applied. Sending success result."
          pure . addHeader @"Location" "/" $ applyCookies NoContent
    -- TODO This is wrong: if UserLoginWithUserName doesn't find a user, it
    -- throws an error instead of returning a Nothing. So either change its
    -- logic to return a Nothing or an empty list, or catch the exception.
    Nothing -> do
      ML.info "User not found. Sending Failure result."
      unauthdErr _userCredsName
 where
  env               = ML.localEnv (<> "HTTP" <> "Login")
  findMatchingUsers = do
    ML.info $ "Logging in user: " <> show _userCredsName <> "..."
    S.dbSelect $ User.UserLoginWithUserName _userCredsName _userCredsPassword
  unauthdErr =
    Errs.throwError'
      . User.IncorrectPassword
      . mappend "User login failed: "
      . show


--------------------------------------------------------------------------------
-- | Serve the static files for the documentation. This also provides a custom
-- 404 fallback.
serveDocumentation root = serveDirectoryWith settings
 where
  settings = (defaultWebAppSettings root)
    { ss404Handler = Just custom404
    , ssLookupFile = webAppLookup hashFileIfExists root
    }

custom404 :: Application
custom404 _request sendResponse = sendResponse $ Wai.responseLBS
  HTTP.status404
  [("Content-Type", "text/html; charset=UTF-8")]
  (renderMarkup $ H.toMarkup Pages.NotFoundPage)


--------------------------------------------------------------------------------
-- Most of this is taken from WaiAppStatic.Storage.Filesystem.
-- The goal is to allow to re-define the above `ssLookupFile` in
-- `defaultWebAppSettings`: instead of looking up files as-is, we make it so
-- that given a URL such as /some/path, we act as if it was /some/path.html,
-- thus making nicer URLs (where the .html is not necessary).
-- TODO Move this to hypered/commence.
-- TODO Don't rewrite paths when there is an extension, otherwise it won't work
-- for e.g. images.
-- Note: we copy more code than necessary because it is not in the export list
-- of the original module.

-- | Construct a new path from a root and some @Pieces@.
pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (\fp p -> fp </> T.unpack (fromPiece p))

-- | Construct a @Piece@ without input validation.
-- The original can use a non-exported `Piece` constructor. Here we use the one
-- that does some validation, but in an unsafe way.
unsafeToPiece :: Text -> Piece
unsafeToPiece t = let Just p = toPiece t in p

-- | More efficient than @fileSystemLookup@ as it only concerns itself with
-- finding files, not folders.
-- This is the main change compared to WaiAppStatic.Storage.Filesystem:
-- If the last piece is empty, this lookup index.html.
-- Otherwise, this appends ".html" before trying to lookup that.
-- This means that if you want `documentation/` to be a directory, you can
-- decide if you want to support `documentation/`, by putting an `index.html`
-- within, or support `documentation`, by making a `documentation.html` file
-- instead.
webAppLookup :: ETagLookup -> FilePath -> Pieces -> IO LookupResult
webAppLookup hashFunc prefix pieces = fileHelperLR hashFunc fp lastPiece
 where
  fp      = pathFromPieces prefix pieces'
  pieces' = initPieces ++ [lastPiece]
  (initPieces, lastPiece)
    | null pieces
    = ([], unsafeToPiece "index.html")
    | Just (last pieces) == toPiece ""
    = (init pieces, unsafeToPiece "index.html")
    | otherwise
    = let lastP = case fromPiece (last pieces) of
            s | T.isSuffixOf ".txt" s -> last pieces
            s                         -> unsafeToPiece $ s <> ".html"
      in  (init pieces, lastP)

-- | Convenience wrapper for @fileHelper@.
fileHelperLR
  :: ETagLookup
  -> FilePath -- ^ file location
  -> Piece -- ^ file name
  -> IO LookupResult
fileHelperLR a b c = fmap (maybe LRNotFound LRFile) $ fileHelper a b c

-- | Attempt to load up a @File@ from the given path.
-- This is taken and modified from WaiAppStatic.Storage.Filesystem.
fileHelper
  :: ETagLookup
  -> FilePath -- ^ file location
  -> Piece -- ^ file name
  -> IO (Maybe File)
fileHelper hashFunc fp name = do
  efs <- try $ getFileStatus fp
  case efs of
    Left (_ :: SomeException)   -> return Nothing
    Right fs | isRegularFile fs -> return $ Just File
      { fileGetSize     = fromIntegral $ fileSize fs
      , fileToResponse  = \s h -> Wai.responseFile s h fp Nothing
      , fileName        = name
      , fileGetHash     = hashFunc fp
      , fileGetModified = Just $ modificationTime fs
      }
    Right _ -> return Nothing

hashFileIfExists :: ETagLookup
hashFileIfExists fp = do
  res <- try $ hashFile fp
  return $ case res of
    Left  (_ :: SomeException) -> Nothing
    Right x                    -> Just x

-- | MD5 hash and base64-encode the file contents. Does not check if the file
-- exists.
hashFile :: FilePath -> IO ByteString
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
  f <- BL.hGetContents h
  let !hash = hashlazy f :: Digest MD5
  return $ convertToBase Base64 hash
