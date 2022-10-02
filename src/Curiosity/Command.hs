{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Curiosity.Command
  ( Command(..)
  , QueueName(..)
  , Queues(..)
  , ParseConf(..)
  , CommandWithTarget(..)
  , CommandTarget(..)
  , CommandUser(..)
  , ObjectType(..)
  , parserInfo
  , parserInfoWithTarget
  ) where

import qualified Commence.Runtime.Storage      as S
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Data.Text                     as T
import qualified Options.Applicative           as A


--------------------------------------------------------------------------------
-- | Describes the command available from the command-line with `cty`, or
-- within the UNIX-domain socket server, `cty-sock`, or the `cty-repl-2` REPL.
data Command =
    Layout
    -- ^ Display the routing layout of the web server.
  | Init
    -- ^ Initialise a new, empty state file.
  | Reset P.Conf
    -- ^ Set the state file to the empty state.
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
  | CreateBusinessEntity Business.Create
  | UpdateBusinessEntity Business.Update
  | CreateLegalEntity Legal.Create
  | UpdateLegalEntity Legal.Update
  | CreateUser User.Signup
  | SelectUser Bool User.UserId Bool
    -- ^ Show a given user. If True, use Haskell format instead of JSON. If
    -- True, show only the user ID and username.
  | FilterUsers User.Predicate
  | UpdateUser (S.DBUpdate User.UserProfile)
  | SetUserEmailAddrAsVerified User.UserName
    -- ^ High-level operations on users.
  | SignQuotation Quotation.QuotationId
  | CreateEmployment Employment.CreateContractAll
  | CreateInvoice
  | EmitInvoice Order.OrderId
    -- ^ Generate and invoice and send an email with it (or a link to it).
  | MatchPayment Text
    -- ^ Notify the system that a payment matching an invoice was done.
  | SendReminder Text
    -- ^ Send an email to remind of an unpaid invoice.
  | FormNewQuotation Quotation.CreateQuotationAll
    -- ^ Create a new instance of the quotation creation form.
  | FormValidateQuotation Text
    -- ^ Run validation rules only (the same ones used in `FormSubmitQuotation`).
  | FormSubmitQuotation Quotation.SubmitQuotation
    -- ^ Submit a quotation.
  | FormNewSimpleContract SimpleContract.CreateContractAll'
  | FormValidateSimpleContract Text
  | ViewQueue QueueName
    -- ^ View queue. The queues can be filters applied to objects, not
    -- necessarily explicit list in database.
  | ViewQueues Queues
  | Step
    -- ^ Execute the next automated action when using stepped (non-wallclock)
    -- mode, or mixed-mode, or the next automated action when the automation is
    -- "disabled".
  | Log Text P.Conf
    -- Log a line of text to the logs.
  | ShowId Text
    -- ^ If not a command per se, assume it's an ID to be looked up.
  deriving (Eq, Show)

data QueueName = EmailAddrToVerify
  deriving (Eq, Show)

data Queues = CurrentUserQueues | AllQueues | AutomatedQueues | ManualQueues | UserQueues User.UserName
  deriving (Eq, Show)

data ParseConf =
    ConfCommand Text
  | ConfFileName FilePath
  | ConfStdin
  | ParseObject ObjectType FilePath
  deriving (Eq, Show)

data ObjectType = ParseState | ParseUser
  deriving (Eq, Show)

-- | The same commands, defined above, can be used within the UNIX-domain
-- socket server, `cty-sock`, but also from a real command-line tool, `cty`.
-- In the later case, a user might want to direct the command-line tool to
-- interact with a server, or a local state file. This data type is meant to
-- augment the above commands with such options. In addition, the command is
-- supposed to be performed by the given user. This is to be set by SSH's
-- ForceCommand.
data CommandWithTarget = CommandWithTarget Command CommandTarget CommandUser
  deriving (Eq, Show)

data CommandTarget = MemoryTarget | StateFileTarget FilePath | UnixDomainTarget FilePath
  deriving (Eq, Show)

-- Running a command can be done by passing explicitely a user (this is
-- intended to be used behind SSH, or possibly when administring the system)
-- or, for convenience, by taking the UNIX login from the current session.
data CommandUser = UserFromLogin | User User.UserName
  deriving (Eq, Show)


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
    user <-
      A.option (A.eitherReader (Right . User . User.UserName . T.pack))
      $  A.short 'u'
      <> A.long "user"
      <> A.value UserFromLogin
      <> A.help
           "A username performing this command. If not given, the username is \
           \the current UNIX login name."
      <> A.metavar "USERNAME"
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
    pure $ CommandWithTarget command target user


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "layout"
          ( A.info (parserLayout <**> A.helper)
          $ A.progDesc "Display the routing layout of the web server"
          )

      <> A.command
           "init"
           ( A.info (parserInit <**> A.helper)
           $ A.progDesc "Initialise a new, empty state file"
           )

      <> A.command
           "reset"
           ( A.info (parserReset <**> A.helper)
           $ A.progDesc "Reset a state file to the empty state"
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
           "business"
           ( A.info (parserBusiness <**> A.helper)
           $ A.progDesc "Commands related to business entities"
           )

      <> A.command
           "legal"
           ( A.info (parserLegal <**> A.helper)
           $ A.progDesc "Commands related to legal entities"
           )

      <> A.command
           "user"
           ( A.info (parserUser <**> A.helper)
           $ A.progDesc "User-related commands"
           )

      <> A.command
           "users"
           ( A.info (parserUsers <**> A.helper)
           $ A.progDesc "Users-related commands"
           )

      <> A.command
           "employment"
           ( A.info (parserEmployment <**> A.helper)
           $ A.progDesc "Commands related to employment contracts"
           )

      <> A.command
           "quotation"
           ( A.info (parserQuotation <**> A.helper)
           $ A.progDesc "Commands related to quotations"
           )

      <> A.command
           "invoice"
           ( A.info (parserInvoice <**> A.helper)
           $ A.progDesc "Commands related to invoices"
           )

      <> A.command
           "payment"
           ( A.info (parserPayment <**> A.helper)
           $ A.progDesc "Commands related to payments"
           )

      <> A.command
           "reminder"
           ( A.info (parserReminder <**> A.helper)
           $ A.progDesc "Commands related to reminders"
           )

      <> A.command
           "forms"
           (A.info (parserForms <**> A.helper) $ A.progDesc
             "Fill and validate forms (i.e. data in the staging area)"
           )

      <> A.command
           "queue"
           ( A.info (parserQueue <**> A.helper)
           $ A.progDesc "Display a queue's content"
           )

      <> A.command
           "queues"
           ( A.info (parserQueues <**> A.helper)
           $ A.progDesc "Display all queues content"
           )

      <> A.command
           "step"
           ( A.info (pure Step <**> A.helper)
           $ A.progDesc "Run the next automated action"
           )

      <> A.command
           "log"
           ( A.info (parserLog <**> A.helper)
           $ A.progDesc "Log a message to the logs"
           )
      )
    <|> parserShowId

parserLayout :: A.Parser Command
parserLayout = pure Layout

parserInit :: A.Parser Command
parserInit = pure Init

parserReset :: A.Parser Command
parserReset = Reset <$> P.confParser

parserRepl :: A.Parser Command
parserRepl = Repl <$> P.confParser

parserServe :: A.Parser Command
parserServe = Serve <$> P.confParser <*> P.serverParser

parserRun :: A.Parser Command
parserRun = Run <$> P.confParser <*> A.argument
  A.str
  (A.metavar "FILE" <> A.action "file" <> A.help "Script to run.")

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

parserBusiness :: A.Parser Command
parserBusiness =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateBusinessEntity <**> A.helper)
         $ A.progDesc "Create a new business entity"
         )
    <> A.command
         "update"
         ( A.info (parserUpdateBusinessEntity <**> A.helper)
         $ A.progDesc "Update a business unit"
         )

parserCreateBusinessEntity :: A.Parser Command
parserCreateBusinessEntity = do
  slug <- A.argument
    A.str
    (A.metavar "SLUG" <> A.help "An identifier suitable for URLs")
  name <- A.argument A.str (A.metavar "NAME" <> A.help "A name")
  pure $ CreateBusinessEntity $ Business.Create slug name

parserUpdateBusinessEntity :: A.Parser Command
parserUpdateBusinessEntity = do
  slug        <- argumentUnitSlug
  description <- A.argument A.str (A.metavar "TEXT" <> A.help "A description")
  pure $ UpdateBusinessEntity $ Business.Update slug (Just description)

argumentUnitId = Legal.EntityId <$> A.argument A.str metavarUnitId

metavarUnitId = A.metavar "BENT-ID" <> A.completer complete <> A.help
  "A business unit ID"
  where complete = A.mkCompleter . const $ pure ["LENT-", "LENT-1", "LENT-2"]
        -- TODO I'd like to lookup IDs in the state, but here we don't know
        -- where the state is (it depends on other command-line options).

argumentUnitSlug = A.argument A.str metavarUnitSlug

metavarUnitSlug = A.metavar "SLUG" <> A.completer complete <> A.help
  "A business unit slug"
  where complete = A.mkCompleter . const $ pure ["alpha", "beta", "gamma"]
        -- TODO I'd like to lookup slugs in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserLegal :: A.Parser Command
parserLegal =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateLegalEntity <**> A.helper)
         $ A.progDesc "Create a new legal entity"
         )
    <> A.command
         "update"
         ( A.info (parserUpdateLegalEntity <**> A.helper)
         $ A.progDesc "Update a legal entity"
         )

parserCreateLegalEntity :: A.Parser Command
parserCreateLegalEntity = do
  slug <- A.argument
    A.str
    (A.metavar "SLUG" <> A.help "An identifier suitable for URLs")
  name <- A.argument A.str (A.metavar "NAME" <> A.help "A registration name")
  cbeNumber <- A.argument
    A.str
    (  A.metavar "CBE-NUMBER"
    <> A.help "CBE number (without prefix or leading zero)"
    )
  vatNumber <- A.argument
    A.str
    (A.metavar "VAT-NUMER" <> A.help "VAT number (with prefix and leading zero")
  pure $ CreateLegalEntity $ Legal.Create slug name cbeNumber vatNumber

parserUpdateLegalEntity :: A.Parser Command
parserUpdateLegalEntity = do
  slug        <- argumentEntitySlug
  description <- A.argument A.str (A.metavar "TEXT" <> A.help "A description")
  pure $ UpdateLegalEntity $ Legal.Update slug (Just description)

argumentEntityId = Legal.EntityId <$> A.argument A.str metavarEntityId

metavarEntityId = A.metavar "LENT-ID" <> A.completer complete <> A.help
  "A legal entity ID"
  where complete = A.mkCompleter . const $ pure ["LENT-", "LENT-1", "LENT-2"]
        -- TODO I'd like to lookup IDs in the state, but here we don't know
        -- where the state is (it depends on other command-line options).

argumentEntitySlug = A.argument A.str metavarEntitySlug

metavarEntitySlug = A.metavar "SLUG" <> A.completer complete <> A.help
  "A legal entity slug"
  where complete = A.mkCompleter . const $ pure ["one", "two", "three"]
        -- TODO I'd like to lookup slugs in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserUser :: A.Parser Command
parserUser = A.subparser
  (  A.command
      "create"
      (A.info (parserCreateUser <**> A.helper) $ A.progDesc "Create a new user")
  <> A.command
       "delete"
       (A.info (parserDeleteUser <**> A.helper) $ A.progDesc "Delete a user")
  <> A.command
       "update"
       (A.info (parserUpdateUser <**> A.helper) $ A.progDesc "Update a user")
  <> A.command
       "get"
       (A.info (parserGetUser <**> A.helper) $ A.progDesc "Select a user")
  <> A.command
       "do"
       ( A.info (parserUserLifeCycle <**> A.helper)
       $ A.progDesc "Perform a high-level operation on a user"
       )
  )

parserUsers :: A.Parser Command
parserUsers = do
  predicate <-
    (  A.flag' User.PredicateEmailAddrToVerify
    $  A.long "email-addr-to-verify"
    <> A.help "Show users with an email address to verify."
    )
    <|> (  A.flag' (User.PredicateHas User.CanCreateContracts)
        $  A.long "can-create-contracts"
        <> A.help "Show users with the right to create contracts."
        )
    <|> (  A.flag' (User.PredicateHas User.CanVerifyEmailAddr)
        $  A.long "can-verify-email-addr"
        <> A.help "Show users with the right to verify email addresses."
        )
  pure $ FilterUsers predicate

parserCreateUser :: A.Parser Command
parserCreateUser = do
  username   <- A.argument A.str (A.metavar "USERNAME" <> A.help "A username")
  password   <- A.argument A.str (A.metavar "PASSWORD" <> A.help "A password")
  email <- A.argument A.str (A.metavar "EMAIL" <> A.help "An email address")
  tosConsent <- A.switch
    (  A.long "accept-tos"
    <> A.help "Indicate if the user being created consents to the TOS."
    )
  pure $ CreateUser $ User.Signup username password email tosConsent

parserDeleteUser :: A.Parser Command
parserDeleteUser = UpdateUser . User.UserDelete <$> argumentUserId

parserUpdateUser :: A.Parser Command
parserUpdateUser = do
  uid  <- argumentUserId
  name <- A.argument A.str (A.metavar "NAME" <> A.help "A display name")
  bio  <- A.argument A.str (A.metavar "TEXT" <> A.help "A bio")
  pure $ UpdateUser . User.UserUpdate uid $ User.Update (Just name) (Just bio)

parserGetUser :: A.Parser Command
parserGetUser =
  SelectUser
    <$> A.switch
          (A.long "hs" <> A.help "Use the Haskell format (default is JSON).")
    <*> argumentUserId
    <*> A.switch (A.long "short" <> A.help "Show only the ID and username.")

argumentUserId = User.UserId <$> A.argument A.str metavarUserId

metavarUserId = A.metavar "USER-ID" <> A.completer complete <> A.help
  "A user ID"
  where complete = A.mkCompleter . const $ pure ["USER-", "USER-1", "USER-2"]
        -- TODO I'd like to lookup IDs in the state, but here we don't know
        -- where the state is (it depends on other command-line options).

argumentUserName = User.UserName <$> A.argument A.str metavarUserName

metavarUserName = A.metavar "USERNAME" <> A.completer complete <> A.help
  "A username"
  where complete = A.mkCompleter . const $ pure ["alice", "bob", "charlie"]
        -- TODO I'd like to lookup usernames in the state, but here we don't
        -- know where the state is (it depends on other command-line options).

parserUserLifeCycle :: A.Parser Command
parserUserLifeCycle = A.subparser $ A.command
  "set-email-addr-as-verified"
  ( A.info (p <**> A.helper)
  $ A.progDesc "Perform a high-level operation on a user"
  )
  where p = SetUserEmailAddrAsVerified <$> argumentUserName

parserEmployment :: A.Parser Command
parserEmployment = A.subparser $ A.command
  "create"
  ( A.info (parserCreateEmployment <**> A.helper)
  $ A.progDesc "Create a new employment contract"
  )

parserCreateEmployment :: A.Parser Command
parserCreateEmployment =
  pure $ CreateEmployment Employment.emptyCreateContractAll

parserQuotation :: A.Parser Command
parserQuotation =
  A.subparser
    $  A.command
         "sign"
         ( A.info (parserSignQuotation <**> A.helper)
         $ A.progDesc "Accept (sign) a quotation"
         )

parserSignQuotation :: A.Parser Command
parserSignQuotation = do
  id <- A.argument A.str
                   (A.metavar "QUOTATION-ID" <> A.help "A quotation identifier.")
  pure $ SignQuotation id

parserInvoice :: A.Parser Command
parserInvoice =
  A.subparser
    $  A.command
         "create"
         ( A.info (parserCreateInvoice <**> A.helper)
         $ A.progDesc "Create a new invoice"
         )
    <> A.command
         "emit"
         ( A.info (parserEmitInvoice <**> A.helper)
         $ A.progDesc "Generate and send an invoice"
         )

parserCreateInvoice :: A.Parser Command
parserCreateInvoice = pure CreateInvoice

parserEmitInvoice :: A.Parser Command
parserEmitInvoice = do
  id <- A.strOption $ A.long "from" <> A.metavar "ORDER-ID" <> A.help
    "An order identifier."
  pure $ EmitInvoice $ Order.OrderId id


--------------------------------------------------------------------------------
parserForms :: A.Parser Command
parserForms =
  A.subparser
    $  A.command
         "quotation"
         ( A.info (parserFormQuotation <**> A.helper)
         $ A.progDesc "Fill and submit quotation forms"
         )
    <> A.command
         "simple-contract"
         ( A.info (parserFormSimpleContract <**> A.helper)
         $ A.progDesc "Fill and submit simple contract forms"
         )


--------------------------------------------------------------------------------
parserFormQuotation :: A.Parser Command
parserFormQuotation =
  A.subparser
    $  A.command
         "new"
         ( A.info (parserFormNewQuotation <**> A.helper)
         $ A.progDesc "Create a new form session"
         )
    <> A.command
         "validate"
         ( A.info (parserFormValidateQuotation <**> A.helper)
         $ A.progDesc "Run validation rules against a form"
         )
    <> A.command
         "submit"
         ( A.info (parserFormSubmitQuotation <**> A.helper)
         $ A.progDesc "Submit a form"
         )

parserFormNewQuotation :: A.Parser Command
parserFormNewQuotation = pure $ FormNewQuotation Quotation.emptyCreateQuotationAll

parserFormValidateQuotation :: A.Parser Command
parserFormValidateQuotation = do
  key <- A.argument A.str
                    (A.metavar "KEY" <> A.help "A quotation form identifier.")
  pure $ FormValidateQuotation key

parserFormSubmitQuotation :: A.Parser Command
parserFormSubmitQuotation = do
  key <- A.argument A.str
                    (A.metavar "KEY" <> A.help "A quotation form identifier.")
  pure $ FormSubmitQuotation $ Quotation.SubmitQuotation key


--------------------------------------------------------------------------------
parserPayment :: A.Parser Command
parserPayment = A.subparser $ A.command
  "match"
  ( A.info (parserMatchPayment <**> A.helper)
  $ A.progDesc "Match a payment to an invoice"
  )

parserMatchPayment :: A.Parser Command
parserMatchPayment = do
  id <- A.argument A.str
                   (A.metavar "INVOICE-ID" <> A.help "An invoice identifier.")
  pure $ MatchPayment id

parserReminder :: A.Parser Command
parserReminder = A.subparser $ A.command
  "send"
  ( A.info (parserSendReminder <**> A.helper)
  $ A.progDesc "Send an invoice payment reminder"
  )

parserSendReminder :: A.Parser Command
parserSendReminder = do
  id <- A.argument A.str
                   (A.metavar "INVOICE-ID" <> A.help "An invoice identifier.")
  pure $ SendReminder id


--------------------------------------------------------------------------------
parserFormSimpleContract :: A.Parser Command
parserFormSimpleContract =
  A.subparser
    $  A.command
         "new"
         ( A.info (parserFormNewSimpleContract <**> A.helper)
         $ A.progDesc "Create a new form session"
         )
    <> A.command
         "validate"
         ( A.info (parserFormValidateSimpleContract <**> A.helper)
         $ A.progDesc "Run validation rules against a form"
         )

parserFormNewSimpleContract :: A.Parser Command
parserFormNewSimpleContract = do
  amount <- A.option
    A.auto
    (A.long "amount" <> A.value 0 <> A.help "Invoicing amount." <> A.metavar
      "EURO"
    )
  pure $ FormNewSimpleContract SimpleContract.emptyCreateContractAll'
    { SimpleContract._createContractInvoice' =
      SimpleContract.emptyCreateContractInvoice
        { SimpleContract._createContractAmount = amount
        }
    }

parserFormValidateSimpleContract :: A.Parser Command
parserFormValidateSimpleContract = do
  key <- A.argument
    A.str
    (A.metavar "KEY" <> A.help "A simple contract form identifier.")
  pure $ FormValidateSimpleContract key

-- TODO I'm using subcommands to have all queue names appear in `--help` but
-- the word COMMAND seems wrong:
--
--     cty queue --help
--     Usage: <interactive> queue COMMAND
--       Display a queue's content
--
--     Available options:
--       -h,--help                Show this help text
--
--     Available commands:
--       user-email-addr-to-verify
parserQueue :: A.Parser Command
parserQueue = A.subparser $ A.command
  "user-email-addr-to-verify"
  ( A.info (p <**> A.helper)
  $ A.progDesc
      "Show users with an email address that need verification. \
      \Use `cty user do set-email-addr-as-verified` to mark an email address \
      \as verified."
  )
  where p = pure $ ViewQueue EmailAddrToVerify

parserQueues :: A.Parser Command
parserQueues =
  ViewQueues
    <$> (   (A.flag' CurrentUserQueues $ A.long "current-user" <> A.help
              "Display the queues of the current user. This is the default."
            )
        <|> (A.flag' AllQueues $ A.long "all" <> A.help
              "Display all the queues of the system."
            )
        <|> (  A.flag' AutomatedQueues
            $  A.long "automated"
            <> A.help
                 "Display the queues that can be handled automatically by the system."
            )
        <|> (A.flag' ManualQueues $ A.long "manual" <> A.help
              "Display the queues that can be handled by users."
            )
        <|> (  A.option
                (A.eitherReader (Right . UserQueues . User.UserName . T.pack))
            $  A.long "from"
            <> A.value CurrentUserQueues
            <> A.help "Display the queues of the give user."
            <> A.metavar "USERNAME"
            )
        )

parserLog :: A.Parser Command
parserLog =
  Log
    <$> A.argument A.str (A.metavar "MESSAGE" <> A.help "A line of text")
    <*> P.confParser

parserShowId :: A.Parser Command
parserShowId =
  ShowId <$> A.argument A.str (A.metavar "ID" <> A.help "An object ID")
