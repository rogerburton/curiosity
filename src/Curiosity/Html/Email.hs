{- |
Module: Curiosity.Html.Email
Description: Page and components to display emails.
-}
module Curiosity.Html.Email
  ( EmailPage(..)
  , panelSentEmails
  ) where

import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Misc               as Misc
import           Text.Blaze.Html5               ( Html )
import qualified Text.Blaze.Html5              as H


--------------------------------------------------------------------------------
-- | The page displaye at @/emails@ to show all enqueued emails.
data EmailPage = EmailPage
  { _emailPageUser   :: Maybe User.UserProfile
    -- ^ The logged-in user, if any.
  , _emailPageEmails :: [Email.Email]
    -- ^ All enqueued emails.
  }

instance H.ToMarkup EmailPage where
  toMarkup (EmailPage mprofile emails) =
    renderView' mprofile $ panelSentEmails emails


--------------------------------------------------------------------------------
-- | Display emails.
panelSentEmails :: [Email.Email] -> Html
panelSentEmails emails =
  panel' "Emails" $ Misc.table "emails" titles display emails
 where
  titles = ["ID", "Template", "Sender", "Recipient", "State"]
  display Email.Email {..} =
    ( [ Email.unEmailId _emailId
      , Email.emailTemplateName _emailTemplate
      , User.unUserEmailAddr _emailSender
      , User.unUserEmailAddr _emailRecipient
      , case _emailState of
          Email.EmailTodo -> "TODO"
          Email.EmailDone -> "DONE"
      ]
    , []
    , Nothing
    )
