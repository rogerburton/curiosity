{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module: Curiosity.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Curiosity.Server
  ( App
    -- * Top level server types.
  , serverT
  , serve
  , routingLayout
  , run

    -- * Type-aliases for convenience
  , ServerSettings
  ) where

import qualified Commence.JSON.Pretty          as JP
import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import qualified Commence.Server.Auth          as CAuth
import           Control.Lens
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified Curiosity.Data                as Data
import           Curiosity.Data                 ( HaskDb
                                                , readFullStmDbInHask
                                                )
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Form.Login          as Login
import qualified Curiosity.Form.Signup         as Signup
import qualified Curiosity.Html.Action         as Pages
import qualified Curiosity.Html.Business       as Pages
import qualified Curiosity.Html.Employment     as Pages
import qualified Curiosity.Html.Errors         as Pages
import qualified Curiosity.Html.Homepage       as Pages
import qualified Curiosity.Html.Invoice        as Pages
import qualified Curiosity.Html.LandingPage    as Pages
import qualified Curiosity.Html.Legal          as Pages
import qualified Curiosity.Html.Profile        as Pages
import qualified Curiosity.Parse               as Command
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server.Helpers      as H
import           Data.Aeson                     ( FromJSON
                                                , Value
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.WebSockets.Connection
import           Prelude                 hiding ( Handler )
import           Servant                 hiding ( serve )
import           Servant.API.WebSocket
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Servant.Server                as Server
import           Smart.Server.Page              ( PageEither )
import qualified Smart.Server.Page             as SS.P
import           System.FilePath                ( (</>) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Renderer.Text      as R
                                                ( renderMarkup )
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )
import           WaiAppStatic.Storage.Filesystem
                                                ( defaultWebAppSettings )
import           WaiAppStatic.Storage.Filesystem.Extended
                                                ( hashFileIfExists
                                                , webAppLookup
                                                )
import           WaiAppStatic.Types             ( ss404Handler
                                                , ssLookupFile
                                                )


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | This is the main Servant API definition for Curiosity.
type App = H.UserAuthentication :> Get '[B.HTML] (PageEither
               Pages.LandingPage
               Pages.WelcomePage
             )
             :<|> "forms" :> "login" :> Get '[B.HTML] Login.Page
             :<|> "forms" :> "signup" :> Get '[B.HTML] Signup.Page
             :<|> "forms" :> "profile" :> Get '[B.HTML] Pages.ProfilePage
             :<|> "forms" :> "new-contract" :> Get '[B.HTML] Pages.CreateContractPage
             :<|> "forms" :> "edit-contract"
                  :> Capture "key" Text
                  :> Get '[B.HTML] Pages.CreateContractPage
             :<|> "forms" :> "confirm-contract"
                  :> Capture "key" Text
                  :> Get '[B.HTML] Pages.ConfirmContractPage

             :<|> "views" :> "profile"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.ProfileView
             :<|> "views" :> "entity"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.EntityView
             :<|> "views" :> "unit"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.UnitView
             :<|> "views" :> "contract"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.ContractView
             :<|> "views" :> "invoice"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.InvoiceView

             :<|> "messages" :> "signup" :> Get '[B.HTML] Signup.SignupResultPage

             :<|> "state" :> Get '[B.HTML] Pages.EchoPage
             :<|> "state.json" :> Get '[JSON] (JP.PrettyJSON '[ 'JP.DropNulls] (HaskDb Rt.Runtime))

             :<|> "echo" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.Login
                  :> Post '[B.HTML] Pages.EchoPage
             :<|> "echo" :> "signup"
                  :> ReqBody '[FormUrlEncoded] User.Signup
                  :> Post '[B.HTML] Pages.EchoPage
             :<|> "echo" :> "new-contract"
                  :> ReqBody '[FormUrlEncoded] Employment.CreateContract
                  :> Verb 'POST 303 '[JSON] ( Headers '[ Header "Location" Text ]
                                              NoContent
                                            )
             :<|> "echo" :> "save-contract"
                  :> Capture "key" Text
                  :> ReqBody '[FormUrlEncoded] Employment.CreateContract
                  :> Verb 'POST 303 '[JSON] ( Headers '[ Header "Location" Text ]
                                              NoContent
                                            )
             :<|> "echo" :> "submit-contract"
                  :> ReqBody '[FormUrlEncoded] Employment.SubmitContract
                  :> Post '[B.HTML] Pages.EchoPage

             :<|> "partials" :> "username-blocklist" :> Get '[B.HTML] H.Html
             :<|> "partials" :> "username-blocklist.json" :> Get '[JSON] [User.UserName]

             :<|> "login" :> Get '[B.HTML] Login.Page
             :<|> "signup" :> Get '[B.HTML] Signup.Page

             :<|> "action" :> "set-email-addr-as-verified"
                  :> Capture "username" User.UserName
                  :> Get '[B.HTML] Pages.SetUserEmailAddrAsVerifiedPage
             :<|> Public
             :<|> Private
             :<|> "data" :> Raw
             :<|> "ubl" :> Capture "schema" Text
                  :> Capture "filename" FilePath :> Get '[JSON] Value
             :<|> "errors" :> "500" :> Get '[B.HTML, JSON] Text
             -- TODO Make a single handler for any namespace:
             :<|> "alice" :> Get '[B.HTML] Pages.PublicProfileView
             :<|> "alice+" :> Get '[B.HTML] Pages.ProfileView
             :<|> WebSocketApi
             :<|> Raw -- Catchall for static files (documentation)
                      -- and for a custom 404

-- | This is the main Servant server definition, corresponding to @App@.
serverT
  :: forall m
   . ServerC m
  => Command.ServerConf
  -> SAuth.JWTSettings
  -> FilePath
  -> FilePath
  -> ServerT App m
serverT conf jwtS root dataDir =
  showHomePage
    :<|> documentLoginPage
    :<|> documentSignupPage
    :<|> documentEditProfilePage
    :<|> documentCreateContractPage
    :<|> documentEditContractPage
    :<|> documentConfirmContractPage

    :<|> documentProfilePage dataDir
    :<|> documentEntityPage dataDir
    :<|> documentUnitPage dataDir
    :<|> documentContractPage dataDir
    :<|> documentInvoicePage dataDir
    :<|> messageSignupSuccess
    :<|> showState
    :<|> showStateAsJson

    :<|> echoLogin
    :<|> echoSignup
    :<|> echoNewContract
    :<|> echoSaveContract
    :<|> echoSubmitContract

    :<|> partialUsernameBlocklist
    :<|> partialUsernameBlocklistAsJson
    :<|> showLoginPage
    :<|> showSignupPage
    :<|> showSetUserEmailAddrAsVerifiedPage
    :<|> publicT conf jwtS
    :<|> privateT conf
    :<|> serveData dataDir
    :<|> serveUBL dataDir
    :<|> serveErrors
    :<|> serveNamespace "alice"
    :<|> serveNamespaceDocumentation "alice"
    :<|> websocket
    :<|> serveDocumentation root


--------------------------------------------------------------------------------
-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be
-- able to run a server.
type ServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m STM User.UserProfile
    , S.DBTransaction m STM
    , MonadReader Rt.Runtime m
    , MonadIO m
    , Show (S.DBError m STM User.UserProfile)
    , S.Db m STM User.UserProfile ~ Data.StmDb Rt.Runtime
    )


--------------------------------------------------------------------------------
type ServerSettings
  = '[SAuth.CookieSettings , SAuth.JWTSettings , ErrorFormatters]

-- | This is the main function of this module. It runs a Warp server, serving
-- our @App@ API definition.
run
  :: forall m
   . MonadIO m
  => Command.ServerConf
  -> Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
run conf@Command.ServerConf {..} runtime = liftIO $ do
  jwk <- SAuth.generateKey
  let jwtSettings = _serverMkJwtSettings jwk
  Warp.run _serverPort $ waiApp jwtSettings
 where
  waiApp jwtS = serve @Rt.AppM (Rt.appMHandlerNatTrans runtime)
                               conf
                               (ctx jwtS)
                               jwtS
                               _serverStaticDir
                               _serverDataDir
  ctx jwtS =
    _serverCookie
      Server.:. jwtS
      Server.:. errorFormatters
      Server.:. Server.EmptyContext

-- | Turn our @serverT@ definition into a Wai application, suitable for
-- Warp.run.
serve
  :: forall m
   . ServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an
                                   -- arbitrary @m@ to a Servant @Handler@
  -> Command.ServerConf
  -> Server.Context ServerSettings
  -> SAuth.JWTSettings
  -> FilePath
  -> FilePath
  -> Wai.Application
serve handlerNatTrans conf ctx jwtS root dataDir =
  Servant.serveWithContext appProxy ctx
    $ hoistServerWithContext appProxy settingsProxy handlerNatTrans
    $ serverT conf jwtS root dataDir
 where
  appProxy      = Proxy @App
  settingsProxy = Proxy @ServerSettings

routingLayout :: forall m . MonadIO m => m Text
routingLayout = do
  let Command.ServerConf {..} = Command.defaultServerConf
  jwk <- liftIO $ SAuth.generateKey
  let jwtSettings = _serverMkJwtSettings jwk
  let ctx =
        _serverCookie
          Server.:. jwtSettings
          Server.:. errorFormatters
          Server.:. Server.EmptyContext
  pure $ layoutWithContext (Proxy @App) ctx


--------------------------------------------------------------------------------
-- | Show the landing page when the user is not logged in, or the welcome page
-- when the user is logged in.
showHomePage
  :: ServerC m
  => SAuth.AuthResult User.UserId
  -> m (PageEither Pages.LandingPage Pages.WelcomePage)
     -- We don't use SS.P.Public Void, nor SS.P.Public 'Authd UserProfile
     -- to not have the automatic heading.
showHomePage authResult = withMaybeUser
  authResult
  (\_ -> pure $ SS.P.PageL Pages.LandingPage)
  (\profile -> do
    Rt.Runtime {..} <- ask
    b <- liftIO . atomically $ Rt.canPerform 'User.SetUserEmailAddrAsVerified
                                             _rDb
                                             profile
    profiles <- if b
      then
        Just
          <$> Rt.withRuntimeAtomically Rt.filterUsers
                                       User.PredicateEmailAddrToVerify
      else pure Nothing
    pure . SS.P.PageR $ Pages.WelcomePage profile profiles
  )


--------------------------------------------------------------------------------
showSignupPage :: ServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: ServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

messageSignupSuccess :: ServerC m => m Signup.SignupResultPage
messageSignupSuccess = pure Signup.SignupSuccess

echoSignup :: ServerC m => User.Signup -> m Pages.EchoPage
echoSignup input = pure $ Pages.EchoPage $ show input


--------------------------------------------------------------------------------
showSetUserEmailAddrAsVerifiedPage
  :: ServerC m => User.UserName -> m Pages.SetUserEmailAddrAsVerifiedPage
showSetUserEmailAddrAsVerifiedPage username =
  withUserFromUsername username (pure . Pages.SetUserEmailAddrAsVerifiedPage)


--------------------------------------------------------------------------------
partialUsernameBlocklist :: ServerC m => m H.Html
partialUsernameBlocklist =
  pure . H.ul $ mapM_ (H.li . H.code . H.toHtml) User.usernameBlocklist

partialUsernameBlocklistAsJson :: ServerC m => m [User.UserName]
partialUsernameBlocklistAsJson = pure User.usernameBlocklist

--------------------------------------------------------------------------------
showLoginPage :: ServerC m => m Login.Page
showLoginPage = pure $ Login.Page "/a/login"

documentLoginPage :: ServerC m => m Login.Page
documentLoginPage = pure $ Login.Page "/echo/login"

echoLogin :: ServerC m => User.Login -> m Pages.EchoPage
echoLogin = pure . Pages.EchoPage . show


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | A publicly available login page.
type Public =    "a" :> "signup"
                 :> ReqBody '[FormUrlEncoded] User.Signup
                 :> Post '[B.HTML] Signup.SignupResultPage
            :<|> "a" :> "login"
                 :> ReqBody '[FormUrlEncoded] User.Login
                 :> Verb 'POST 303 '[JSON] ( Headers CAuth.PostAuthHeaders
                                              NoContent
                                            )

publicT
  :: forall m
   . ServerC m
  => Command.ServerConf
  -> SAuth.JWTSettings
  -> ServerT Public m
publicT conf jwtS = handleSignup :<|> handleLogin conf jwtS

handleSignup
  :: forall m
   . (ServerC m, Show (S.DBError m STM User.UserProfile))
  => User.Signup
  -> m Signup.SignupResultPage
handleSignup input@User.Signup {..} =
  ML.localEnv (<> "HTTP" <> "Signup")
    $   do
          ML.info $ "Signing up new user: " <> show username <> "..."
          db <- asks Rt._rDb
          S.liftTxn @m @STM
            $ S.dbUpdate @m db (User.UserCreateGeneratingUserId input)
          -- Rt.withRuntimeAtomically Rt.createUser input
    >>= \case
          Right uid -> do
            ML.info
              $  "User created: "
              <> show uid
              <> ". Sending success result."
            pure Signup.SignupSuccess
          Left err -> do
            ML.info
              $  "Failed to create a user. Sending failure result: "
              <> show err
            Errs.throwError' err

handleLogin
  :: forall m
   . ServerC m
  => Command.ServerConf
  -> SAuth.JWTSettings
  -> User.Login
  -> m (Headers CAuth.PostAuthHeaders NoContent)
handleLogin conf jwtSettings input =
  ML.localEnv (<> "HTTP" <> "Login")
    $   do
          ML.info
            $  "Logging in user: "
            <> show (User._loginUsername input)
            <> "..."
          let credentials = User.Credentials (User._loginUsername input)
                                             (User._loginPassword input)
          db <- asks Rt._rDb
          S.liftTxn @m @STM (Rt.checkCredentials db credentials)
    >>= \case
          Right (Just u) -> do
            ML.info "Found user, applying authentication cookies..."
            -- TODO I think jwtSettings could be retrieved with
            -- Servant.Server.getContetEntry. This would avoid threading
            -- jwtSettings evereywhere.
            mApplyCookies <- liftIO $ SAuth.acceptLogin
              (Command._serverCookie conf)
              jwtSettings
              (u ^. User.userProfileId)
            case mApplyCookies of
              Nothing -> do
                -- From a quick look at Servant, it seems the error would be a
                -- JSON-encoding failure of the generated JWT.
                let err = ServerErr "Couldn't apply cookies."
                ML.error
                  $  "Applying cookies failed. Sending failure result: "
                  <> show err
                Errs.throwError' err
              Just applyCookies -> do
                ML.info "Cookies applied. Sending success result."
                pure . addHeader @"Location" "/" $ applyCookies NoContent
          Right Nothing -> reportErr User.IncorrectUsernameOrPassword
          Left  err     -> reportErr err
 where
  reportErr err = do
    ML.info
      $  "Incorrect username or password. Sending failure result: "
      <> show err
    Errs.throwError' err


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | The private API with authentication.
type Private = H.UserAuthentication :> (
                   "settings" :> "profile"
                   :> Get '[B.HTML] Pages.ProfileView
             :<|>  "settings" :> "profile.json"
                   :> Get '[JSON] User.UserProfile
             :<|>  "settings" :> "profile" :> "edit"
                   :> Get '[B.HTML] Pages.ProfilePage

             :<|> "new" :> "entity" :> Get '[B.HTML] Pages.CreateEntityPage
             :<|> "new" :> "unit" :> Get '[B.HTML] Pages.CreateUnitPage
             :<|> "new" :> "contract" :> Get '[B.HTML] Pages.CreateContractPage
             :<|> "new" :> "invoice" :> Get '[B.HTML] Pages.CreateInvoicePage

             :<|>  "a" :>"set-user-profile"
                   :> ReqBody '[FormUrlEncoded] User.Update
                   :> H.PostUserPage Pages.ProfileSaveConfirmPage
             :<|> "a" :> "logout" :> Verb 'GET 303 '[JSON] ( Headers CAuth.PostLogoutHeaders
                                                             NoContent
                                                            )
             :<|> "a" :> "set-email-addr-as-verified"
                   :> ReqBody '[FormUrlEncoded] User.SetUserEmailAddrAsVerified
                   :> Post '[B.HTML] Pages.ActionResult
  )

privateT :: forall m . ServerC m => Command.ServerConf -> ServerT Private m
privateT conf authResult =
  let withUser' :: forall m a . ServerC m => (User.UserProfile -> m a) -> m a
      withUser' = withUser authResult
  in  (withUser' showProfilePage)
        :<|> (withUser' showProfileAsJson)
        :<|> (withUser' showEditProfilePage)
        :<|> (withUser' showCreateEntityPage)
        :<|> (withUser' showCreateUnitPage)
        :<|> (withUser' showCreateContractPage)
        :<|> (withUser' showCreateInvoicePage)
        :<|> (withUser' . handleUserUpdate)
        :<|> (withUser' $ const (handleLogout conf))
        :<|> (withUser' . handleSetUserEmailAddrAsVerified)

--------------------------------------------------------------------------------
-- | Handle a user's logout.
handleLogout
  :: forall m
   . ServerC m
  => Command.ServerConf
  -> m (Headers CAuth.PostLogoutHeaders NoContent)
handleLogout conf = pure . addHeader @"Location" "/" $ SAuth.clearSession
  (Command._serverCookie conf)
  NoContent

showProfilePage profile =
  pure $ Pages.ProfileView profile (Just "/settings/profile/edit")

showProfileAsJson
  :: forall m . ServerC m => User.UserProfile -> m User.UserProfile
showProfileAsJson = pure

showEditProfilePage profile =
  pure $ Pages.ProfilePage profile "/a/set-user-profile"

showCreateEntityPage
  :: ServerC m => User.UserProfile -> m Pages.CreateEntityPage
showCreateEntityPage profile =
  pure $ Pages.CreateEntityPage profile "/a/new-entity"

showCreateUnitPage :: ServerC m => User.UserProfile -> m Pages.CreateUnitPage
showCreateUnitPage profile = pure $ Pages.CreateUnitPage profile "/a/new-unit"

showCreateContractPage
  :: ServerC m => User.UserProfile -> m Pages.CreateContractPage
showCreateContractPage profile = pure $ Pages.CreateContractPage
  profile
  Employment.emptyCreateContract
  "/a/new-contract"

showCreateInvoicePage
  :: ServerC m => User.UserProfile -> m Pages.CreateInvoicePage
showCreateInvoicePage profile =
  pure $ Pages.CreateInvoicePage profile "/a/new-invoice"


--------------------------------------------------------------------------------
handleUserUpdate
  :: forall m
   . ServerC m
  => User.Update
  -> User.UserProfile
  -> m
       ( SS.P.Page
           'SS.P.Authd
           User.UserProfile
           Pages.ProfileSaveConfirmPage
       )
handleUserUpdate User.Update {..} profile = case _editPassword of
  Just newPass -> do
    let updatedProfile =
          profile
            &  User.userProfileCreds
            .  User.userCredsPassword
            %~ (`fromMaybe` _editPassword)
    db   <- asks Rt._rDb
    eIds <- S.liftTxn
      (S.dbUpdate @m @STM db (User.UserPasswordUpdate (S.dbId profile) newPass))

    let page = case eIds of
          Right (Right [_]) -> Pages.ProfileSaveSuccess
          _ -> Pages.ProfileSaveFailure (Just "Cannot update the user.")
    pure $ SS.P.AuthdPage updatedProfile page

  Nothing -> pure . SS.P.AuthdPage profile . Pages.ProfileSaveFailure $ Just
    "Nothing to update."

handleSetUserEmailAddrAsVerified
  :: forall m
   . ServerC m
  => User.SetUserEmailAddrAsVerified
  -> User.UserProfile
  -> m Pages.ActionResult
handleSetUserEmailAddrAsVerified (User.SetUserEmailAddrAsVerified username) profile
  = do
    db     <- asks Rt._rDb
    output <- liftIO . atomically $ Rt.setUserEmailAddrAsVerifiedFull
      db
      (profile, username)
    pure $ Pages.ActionResult "Set email address as verified" $ case output of
      Right ()  -> "Success"
      Left  err -> "Failure: " <> show err

documentEditProfilePage :: ServerC m => m Pages.ProfilePage
documentEditProfilePage = do
  profile <- readJson "data/alice.json"
  pure $ Pages.ProfilePage profile "/echo/profile"

documentCreateContractPage :: ServerC m => m Pages.CreateContractPage
documentCreateContractPage = do
  profile <- readJson "data/alice.json"
  let contract = Employment.emptyCreateContract
  pure $ Pages.CreateContractPage profile contract "/echo/new-contract"

-- | Same as documentCreateContractPage, but use an existing form.
documentEditContractPage :: ServerC m => Text -> m Pages.CreateContractPage
documentEditContractPage key = do
  profile <- readJson "data/alice.json"
  db      <- asks Rt._rDb
  output  <- liftIO . atomically $ Rt.readCreateContractForm db (profile, key)
  case output of
    Right contract -> pure $ Pages.CreateContractPage
      profile
      contract
      (H.toValue $ "/echo/save-contract/" <> key)
    Left _ -> Errs.throwError' . Rt.FileDoesntExistErr $ T.unpack key -- TODO Specific error.

-- | Save a form, generating a new key.
echoNewContract
  :: ServerC m
  => Employment.CreateContract
  -> m (Headers '[Header "Location" Text] NoContent)
echoNewContract c@Employment.CreateContract {..} = do
  profile <- readJson "data/alice.json"
  db      <- asks Rt._rDb
  key     <- liftIO . atomically $ Rt.newCreateContractForm db (profile, c)
  pure $ addHeader @"Location" ("/forms/confirm-contract/" <> key) NoContent

-- | Save a form, re-using a key.
echoSaveContract
  :: ServerC m
  => Text
  -> Employment.CreateContract
  -> m (Headers '[Header "Location" Text] NoContent)
echoSaveContract key c@Employment.CreateContract {..} = do
  profile <- readJson "data/alice.json"
  db <- asks Rt._rDb
  todo <- liftIO . atomically $ Rt.writeCreateContractForm db (profile, key, c)
  pure $ addHeader @"Location" ("/forms/confirm-contract/" <> key) NoContent

documentConfirmContractPage :: ServerC m => Text -> m Pages.ConfirmContractPage
documentConfirmContractPage key = do
  profile <- readJson "data/alice.json"
  db      <- asks Rt._rDb
  output  <- liftIO . atomically $ Rt.readCreateContractForm db (profile, key)
  case output of
    Right contract -> pure
      $ Pages.ConfirmContractPage profile key contract "/echo/submit-contract"
    Left _ -> Errs.throwError' . Rt.FileDoesntExistErr $ T.unpack key -- TODO Specific error.

echoSubmitContract
  :: ServerC m => Employment.SubmitContract -> m Pages.EchoPage
echoSubmitContract (Employment.SubmitContract key) = do
  profile <- readJson "data/alice.json"
  db      <- asks Rt._rDb
  output  <- liftIO . atomically $ Rt.readCreateContractForm db (profile, key)
  case output of
    Right contract -> pure . Pages.EchoPage $ show contract
    Left  _        -> Errs.throwError' . Rt.FileDoesntExistErr $ T.unpack key -- TODO Specific error.

-- TODO Validate the filename (e.g. this can't be a path going up).
documentProfilePage :: ServerC m => FilePath -> FilePath -> m Pages.ProfileView
documentProfilePage dataDir filename = do
  profile <- readJson $ dataDir </> filename
  pure $ Pages.ProfileView profile (Just "#")


--------------------------------------------------------------------------------
-- TODO Validate the filename (e.g. this can't be a path going up).
documentEntityPage :: ServerC m => FilePath -> FilePath -> m Pages.EntityView
documentEntityPage dataDir filename = do
  entity <- readJson $ dataDir </> filename
  pure $ Pages.EntityView entity (Just "#")


--------------------------------------------------------------------------------
-- TODO Validate the filename (e.g. this can't be a path going up).
documentUnitPage :: ServerC m => FilePath -> FilePath -> m Pages.UnitView
documentUnitPage dataDir filename = do
  unit <- readJson $ dataDir </> filename
  pure $ Pages.UnitView unit (Just "#")


--------------------------------------------------------------------------------
-- TODO Validate the filename (e.g. this can't be a path going up).
documentContractPage
  :: ServerC m => FilePath -> FilePath -> m Pages.ContractView
documentContractPage dataDir filename = do
  contract <- readJson $ dataDir </> filename
  pure $ Pages.ContractView contract (Just "#")


--------------------------------------------------------------------------------
-- TODO Validate the filename (e.g. this can't be a path going up).
documentInvoicePage :: ServerC m => FilePath -> FilePath -> m Pages.InvoiceView
documentInvoicePage dataDir filename = do
  invoice <- readJson $ dataDir </> filename
  pure $ Pages.InvoiceView invoice (Just "#")


--------------------------------------------------------------------------------
-- TODO When given a wrong path, e.g, in documentProfilePage above, the
-- returned HTML is not what I expect, nor the error is logged to our log files.
readJson :: (ServerC m, FromJSON a) => FilePath -> m a
readJson path = do
  content <- liftIO $ BS.readFile path
  let ma = eitherDecode content
  case ma of
    Right a -> pure a
    -- TODO Be more specific, e.g. the error can also be malformed JSON.
    Left  _ -> Errs.throwError' $ Rt.FileDoesntExistErr path


--------------------------------------------------------------------------------
-- | Run a handler, ensuring a user profile can be extracted from the
-- authentication result, or throw an error.
withUser
  :: forall m a
   . ServerC m
  => SAuth.AuthResult User.UserId
  -> (User.UserProfile -> m a)
  -> m a
withUser authResult f = withMaybeUser authResult (authFailedErr . show) f
 where
  authFailedErr = Errs.throwError' . User.UserNotFound . mappend
    "Authentication failed, please login again. Error: "

-- | Run either a handler expecting a user profile, or a normal handler,
-- depending on if a user profile can be extracted from the authentication
-- result or not.
withMaybeUser
  :: forall m a
   . ServerC m
  => SAuth.AuthResult User.UserId
  -> (SAuth.AuthResult User.UserId -> m a)
  -> (User.UserProfile -> m a)
  -> m a
withMaybeUser authResult a f = case authResult of
  SAuth.Authenticated userId -> do
    db <- asks Rt._rDb
    S.liftTxn (S.dbSelect @m @STM db (User.SelectUserById userId))
      <&> (preview $ _Right . _head)
      >>= \case
            Nothing -> do
              ML.warning
                "Cookie-based authentication succeeded, but the user ID is not found."
              authFailedErr $ "No user found with ID " <> show userId
            Just userProfile -> f userProfile
  authFailed -> a authFailed
  where authFailedErr = Errs.throwError' . User.UserNotFound

-- | Run a handler, ensuring a user profile can be extracted from the
-- authentication result, or throw an error.
withUserFromUsername
  :: forall m a
   . ServerC m
  => User.UserName
  -> (User.UserProfile -> m a)
  -> m a
withUserFromUsername username f = withMaybeUserFromUsername
  username
  (noSuchUserErr . show)
  f
 where
  noSuchUserErr = Errs.throwError' . User.UserNotFound . mappend
    "The given username was not found: "

-- | Run either a handler expecting a user profile, or a normal handler,
-- depending on if a user profile can be queried using the supplied username or
-- not.
withMaybeUserFromUsername
  :: forall m a
   . ServerC m
  => User.UserName
  -> (User.UserName -> m a)
  -> (User.UserProfile -> m a)
  -> m a
withMaybeUserFromUsername username a f = do
  mprofile <- Rt.withRuntimeAtomically (Rt.selectUserByUsername . Rt._rDb)
                                       username
  case mprofile of
    Just userProfile -> f userProfile
    Nothing          -> a username


--------------------------------------------------------------------------------
showState :: ServerC m => m Pages.EchoPage
showState = do
  stmDb <- asks Rt._rDb
  db    <- readFullStmDbInHask stmDb
  pure . Pages.EchoPage $ show db

-- TODO The passwords are displayed in clear. Would be great to have the option
-- to hide/show them.
showStateAsJson
  :: ServerC m => m (JP.PrettyJSON '[ 'JP.DropNulls] (HaskDb Rt.Runtime))
showStateAsJson = do
  stmDb <- asks Rt._rDb
  db    <- readFullStmDbInHask stmDb
  pure $ JP.PrettyJSON db


--------------------------------------------------------------------------------
-- | Serve the static files for the documentation. This also provides a custom
-- 404 fallback.
-- serveDocumentation :: FilePath -> Tagged m Application
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
-- | Serve example data as JSON files.
serveData path = serveDirectoryWith settings
 where
  settings = (defaultWebAppSettings path) { ss404Handler = Just custom404 }


--------------------------------------------------------------------------------
-- | Serve example data as UBL JSON files.
-- `schema` can be for instance `"PartyLegalEntity"`.
serveUBL dataDir "PartyLegalEntity" filename = do
  value <- readJson $ dataDir </> filename
  pure . Legal.toUBL $ value

serveUBL _ schema _ =
  Errs.throwError' . Rt.FileDoesntExistErr $ T.unpack schema -- TODO Specific error.


--------------------------------------------------------------------------------
-- | Serve errors intentionally. Only 500 for now.
serveErrors :: ServerC m => m Text
serveErrors = Errs.throwError' $ ServerErr "Intentional 500."

errorFormatters :: Server.ErrorFormatters
errorFormatters = defaultErrorFormatters


--------------------------------------------------------------------------------
-- | Serve the pages under a namespace. TODO Currently the namespace is
-- hard-coded to "alice"
serveNamespace :: ServerC m => User.UserName -> m Pages.PublicProfileView
serveNamespace username =
  withUserFromUsername username (pure . Pages.PublicProfileView)

serveNamespaceDocumentation
  :: ServerC m => User.UserName -> m Pages.ProfileView
serveNamespaceDocumentation username = withUserFromUsername
  username
  (\profile -> pure $ Pages.ProfileView profile Nothing)


--------------------------------------------------------------------------------
-- Accept websocket connections, and keep them alive. This is used by the
-- `autoReload` element to cause a web page to auto-refresh when the connection
-- is lost, e.g. when using ghcid to re-launch the server upon changes to the
-- source code.
type WebSocketApi = "ws" :> WebSocket

websocket :: ServerC m => Connection -> m ()
websocket con =
  liftIO $ withPingThread con 30 (pure ()) $ liftIO . forM_ [1 ..] $ \i -> do
    sendTextData con (show (i :: Int) :: Text)
    threadDelay $ 60 * 1000000 -- 60 seconds


--------------------------------------------------------------------------------
newtype ServerErr = ServerErr Text
  deriving Show

instance Errs.IsRuntimeErr ServerErr where
  errCode ServerErr{} = "ERR.INTERNAL"
  httpStatus ServerErr{} = HTTP.internalServerError500
  userMessage = Just . \case
    ServerErr msg ->
      LT.toStrict . R.renderMarkup . H.toMarkup $ Pages.ErrorPage
        500
        "Internal server error"
        msg
