
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API where

import           Servant
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime, Day)
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))

import           TDF.DTO

type PartyAPI =
       Get '[JSON] [PartyDTO]
  :<|> ReqBody '[JSON] PartyCreate :> Post '[JSON] PartyDTO
  :<|> Capture "partyId" Int64 :> (
           Get '[JSON] PartyDTO
      :<|> ReqBody '[JSON] PartyUpdate :> Put '[JSON] PartyDTO
      :<|> "roles" :> ReqBody '[JSON] Text :> Post '[JSON] NoContent
      )

type BookingAPI =
       Get '[JSON] [BookingDTO]
  :<|> ReqBody '[JSON] CreateBookingReq :> Post '[JSON] BookingDTO

type PackageAPI =
       "products" :> Get '[JSON] [PackageProductDTO]
  :<|> "purchases" :> ReqBody '[JSON] PackagePurchaseReq :> Post '[JSON] NoContent

type InvoiceAPI =
       Get '[JSON] [InvoiceDTO]
  :<|> ReqBody '[JSON] CreateInvoiceReq :> Post '[JSON] InvoiceDTO

type AdminAPI =
       "seed" :> Post '[JSON] NoContent

type HealthAPI = Get '[JSON] HealthStatus

type API =
       "health"  :> HealthAPI
  :<|> "parties" :> PartyAPI
  :<|> "bookings" :> BookingAPI
  :<|> "packages" :> PackageAPI
  :<|> "invoices" :> InvoiceAPI
  :<|> "admin"    :> AdminAPI

data HealthStatus = HealthStatus { status :: String, db :: String }

instance ToJSON HealthStatus where
  toJSON (HealthStatus s d) = object ["status" .= s, "db" .= d]

data CreateBookingReq = CreateBookingReq
  { cbTitle    :: Text
  , cbStartsAt :: UTCTime
  , cbEndsAt   :: UTCTime
  , cbStatus   :: Text
  , cbNotes    :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON CreateBookingReq

data CreateInvoiceReq = CreateInvoiceReq
  { ciCustomerId    :: Int64
  , ciSubtotalCents :: Int
  , ciTaxCents      :: Int
  , ciTotalCents    :: Int
  , ciNumber        :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON CreateInvoiceReq
