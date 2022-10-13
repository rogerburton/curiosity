{- |
Module: Curiosity.Html.Quotation
Description: Quotation pages (view and edit).
-}
module Curiosity.Html.Quotation
  ( QuotationView(..)
  , CreateQuotationPage(..)
  , ConfirmQuotationPage(..)
  ) where

import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data QuotationView = QuotationView
  { _quotationViewQuotation     :: Quotation.Quotation
  , _quotationViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup QuotationView where
  toMarkup (QuotationView quotation hasEditButton) =
    renderView $ quotationView quotation hasEditButton

quotationView quotation hasEditButton = containerLarge $ do
  title' "Quotation" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Quotation._quotationId quotation)


--------------------------------------------------------------------------------
data CreateQuotationPage = CreateQuotationPage
  { _createQuotationPageUserProfile :: User.UserProfile
    -- ^ The user creating the quotation
  , _createQuotationPageKey         :: Maybe Text
    -- ^ The form editing session key, if the form was already saved
  , _createQuotationQuotation       :: Quotation.CreateQuotationAll
  , _createQuotationPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateQuotationPage where
  toMarkup (CreateQuotationPage profile mkey quotation submitUrl) =
    renderForm profile $ groupLayout $ do
      title "New quotation"
      inputText "Quotation name" "name" Nothing Nothing
      submitButton submitUrl
        $ maybe "Create new quotation" (const "Save quotation") mkey


--------------------------------------------------------------------------------
data ConfirmQuotationPage = ConfirmQuotationPage
  { _confirmQuotationPageUserProfile :: User.UserProfile
    -- ^ The user creating the quotation
  , _confirmQuotationPageKey         :: Text
  , _confirmQuotationPageQuotation   :: Quotation.CreateQuotationAll
  , _confirmQuotationPageEditURL     :: Maybe H.AttributeValue
  , _confirmQuotationPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmQuotationPage where
  toMarkup (ConfirmQuotationPage profile key (Quotation.CreateQuotationAll{}) meditUrl submitUrl)
    = renderFormLarge profile $ do
      title' "New quotation" meditUrl

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Submit quotation"
