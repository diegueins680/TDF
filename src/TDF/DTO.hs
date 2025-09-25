{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.DTO where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time (UTCTime)

import           Database.Persist (Entity(..))
import           Database.Persist.Sql (fromSqlKey)

import           TDF.Models

-- Parties
data PartyDTO = PartyDTO
  { partyId          :: Int64
  , legalName        :: Maybe Text
  , displayName      :: Text
  , isOrg            :: Bool
  , taxId            :: Maybe Text
  , primaryEmail     :: Maybe Text
  , primaryPhone     :: Maybe Text
  , whatsapp         :: Maybe Text
  , instagram        :: Maybe Text
  , emergencyContact :: Maybe Text
  , notes            :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON PartyDTO
instance FromJSON PartyDTO

data PartyCreate = PartyCreate
  { cLegalName        :: Maybe Text
  , cDisplayName      :: Text
  , cIsOrg            :: Bool
  , cTaxId            :: Maybe Text
  , cPrimaryEmail     :: Maybe Text
  , cPrimaryPhone     :: Maybe Text
  , cWhatsapp         :: Maybe Text
  , cInstagram        :: Maybe Text
  , cEmergencyContact :: Maybe Text
  , cNotes            :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PartyCreate

data PartyUpdate = PartyUpdate
  { uLegalName        :: Maybe Text
  , uDisplayName      :: Maybe Text
  , uIsOrg            :: Maybe Bool
  , uTaxId            :: Maybe Text
  , uPrimaryEmail     :: Maybe Text
  , uPrimaryPhone     :: Maybe Text
  , uWhatsapp         :: Maybe Text
  , uInstagram        :: Maybe Text
  , uEmergencyContact :: Maybe Text
  , uNotes            :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PartyUpdate

toPartyDTO :: Entity Party -> PartyDTO
toPartyDTO (Entity pid p) = PartyDTO
  { partyId          = fromSqlKey pid
  , legalName        = partyLegalName p
  , displayName      = partyDisplayName p
  , isOrg            = partyIsOrg p
  , taxId            = partyTaxId p
  , primaryEmail     = partyPrimaryEmail p
  , primaryPhone     = partyPrimaryPhone p
  , whatsapp         = partyWhatsapp p
  , instagram        = partyInstagram p
  , emergencyContact = partyEmergencyContact p
  , notes            = partyNotes p
  }

-- Helper
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Bookings
data BookingDTO = BookingDTO
  { bookingId   :: Int64
  , title       :: Text
  , startsAt    :: UTCTime
  , endsAt      :: UTCTime
  , status      :: Text
  , notes       :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON BookingDTO

-- Packages
data PackageProductDTO = PackageProductDTO
  { ppId         :: Int64
  , ppName       :: Text
  , ppService    :: Text
  , ppUnitsKind  :: Text
  , ppUnitsQty   :: Int
  , ppPriceCents :: Int
  } deriving (Show, Generic)
instance ToJSON PackageProductDTO

data PackagePurchaseReq = PackagePurchaseReq
  { buyerId   :: Int64
  , productId :: Int64
  } deriving (Show, Generic)
instance FromJSON PackagePurchaseReq

-- Invoices
data InvoiceDTO = InvoiceDTO
  { invId        :: Int64
  , number       :: Maybe Text
  , statusI      :: Text
  , subtotalC    :: Int
  , taxC         :: Int
  , totalC       :: Int
  } deriving (Show, Generic)
instance ToJSON InvoiceDTO
