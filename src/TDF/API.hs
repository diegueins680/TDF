
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API where

import           Servant
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))

import           TDF.API.Admin     (AdminAPI)
import           TDF.API.Future    (FutureAPI)
import           TDF.API.Bands     (BandsAPI)
import           TDF.API.Inventory (InventoryAPI)
import           TDF.API.Rooms     (RoomsAPI)
import           TDF.API.Sessions  (SessionsAPI)
import           TDF.API.Types     (LooseJSON, RolePayload)
import           TDF.DTO

type PartyAPI =
       Get '[JSON] [PartyDTO]
  :<|> ReqBody '[JSON] PartyCreate :> Post '[JSON] PartyDTO
      :<|> Capture "partyId" Int64 :> (
           Get '[JSON] PartyDTO
      :<|> ReqBody '[JSON] PartyUpdate :> Put '[JSON] PartyDTO
      :<|> "roles" :> ReqBody '[LooseJSON, PlainText, OctetStream] RolePayload :> Post '[JSON] NoContent
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

type HealthAPI = Get '[JSON] HealthStatus

type LoginAPI = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type ProtectedAPI =
       "parties"  :> PartyAPI
  :<|> "bookings" :> BookingAPI
  :<|> "packages" :> PackageAPI
  :<|> "invoices" :> InvoiceAPI
  :<|> "admin"    :> AdminAPI
  :<|> InventoryAPI
  :<|> BandsAPI
  :<|> SessionsAPI
  :<|> RoomsAPI
  :<|> "stubs"    :> FutureAPI

type API =
       "health" :> HealthAPI
  :<|> "login"  :> LoginAPI
  :<|> AuthProtect "bearer-token" :> ProtectedAPI

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
