{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.Server where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Data.Int (Int64)
import           Data.Time (getCurrentTime, UTCTime, Day, utctDay)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Servant
import           Network.Wai (Application)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.DB
import           TDF.Models
import           TDF.DTO
import           TDF.Seed (seedAll)

type AppM = ReaderT Env Handler

mkApp :: Env -> Application
mkApp env = serve (Proxy :: Proxy API) (hoistServer (Proxy :: Proxy API) (nt env) server)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

server :: ServerT API AppM
server =
       health
  :<|> partyServer
  :<|> bookingServer
  :<|> packageServer
  :<|> invoiceServer
  :<|> adminServer

-- Health
health :: AppM TDF.API.HealthStatus
health = pure (HealthStatus "ok" "ok")

-- Parties
partyServer :: ServerT PartyAPI AppM
partyServer = listParties :<|> createParty :<|> partyById
  where
    partyById pid = getParty pid :<|> updateParty pid :<|> addRole pid

listParties :: AppM [PartyDTO]
listParties = do
  Env pool _ <- ask
  entities <- liftIO $ flip runSqlPool pool $ selectList [] [Asc PartyId]
  pure (map toPartyDTO entities)

createParty :: PartyCreate -> AppM PartyDTO
createParty req = do
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let p = Party
          { partyLegalName = cLegalName req
          , partyDisplayName = cDisplayName req
          , partyIsOrg = cIsOrg req
          , partyTaxId = cTaxId req
          , partyPrimaryEmail = cPrimaryEmail req
          , partyPrimaryPhone = cPrimaryPhone req
          , partyWhatsapp = cWhatsapp req
          , partyInstagram = cInstagram req
          , partyEmergencyContact = cEmergencyContact req
          , partyNotes = cNotes req
          , partyCreatedAt = now
          }
  pid <- liftIO $ flip runSqlPool pool $ insert p
  pure $ toPartyDTO (Entity pid p)

getParty :: Int64 -> AppM PartyDTO
getParty pidI = do
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  mp <- liftIO $ flip runSqlPool pool $ getEntity pid
  case mp of
    Nothing -> throwError err404
    Just ent -> pure (toPartyDTO ent)

updateParty :: Int64 -> PartyUpdate -> AppM PartyDTO
updateParty pidI req = do
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  liftIO $ flip runSqlPool pool $ do
    mp <- get pid
    case mp of
      Nothing -> pure ()
      Just p -> do
        let p' = p
              { partyLegalName        = maybe (partyLegalName p) Just (uLegalName req)
              , partyDisplayName      = maybe (partyDisplayName p) id   (uDisplayName req)
              , partyIsOrg            = maybe (partyIsOrg p) id         (uIsOrg req)
              , partyTaxId            = maybe (partyTaxId p) Just       (uTaxId req)
              , partyPrimaryEmail     = maybe (partyPrimaryEmail p) Just (uPrimaryEmail req)
              , partyPrimaryPhone     = maybe (partyPrimaryPhone p) Just (uPrimaryPhone req)
              , partyWhatsapp         = maybe (partyWhatsapp p) Just    (uWhatsapp req)
              , partyInstagram        = maybe (partyInstagram p) Just   (uInstagram req)
              , partyEmergencyContact = maybe (partyEmergencyContact p) Just (uEmergencyContact req)
              , partyNotes            = maybe (partyNotes p) Just       (uNotes req)
              }
        replace pid p'
  getParty pidI

addRole :: Int64 -> Text -> AppM NoContent
addRole pidI roleTxt = do
  Env pool _ <- ask
  let pid  = toSqlKey pidI :: Key Party
      role = parseRole roleTxt
  liftIO $ flip runSqlPool pool $ upsert
    (PartyRole pid role True)
    [ PartyRoleActive =. True ]
  pure NoContent
  where
    parseRole t =
      case readMaybe (T.unpack t) of
        Just r  -> r
        Nothing -> ReadOnly

-- Bookings
bookingServer :: ServerT BookingAPI AppM
bookingServer = listBookings :<|> createBooking

listBookings :: AppM [BookingDTO]
listBookings = do
  Env pool _ <- ask
  bs <- liftIO $ flip runSqlPool pool $ selectList [] [Desc BookingId]
  pure $ map toDTO bs
  where
    toDTO (Entity bid b) = BookingDTO
      { bookingId = fromSqlKey bid
      , title     = bookingTitle b
      , startsAt  = bookingStartsAt b
      , endsAt    = bookingEndsAt b
      , status    = T.pack (show (bookingStatus b))
      , notes     = bookingNotes b
      }

createBooking :: CreateBookingReq -> AppM BookingDTO
createBooking req = do
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let status' = parseStatus (cbStatus req)
      b = Booking
        { bookingTitle          = cbTitle req
        , bookingServiceOrderId = Nothing
        , bookingStartsAt       = cbStartsAt req
        , bookingEndsAt         = cbEndsAt req
        , bookingStatus         = status'
        , bookingCreatedBy      = Nothing
        , bookingNotes          = cbNotes req
        , bookingCreatedAt      = now
        }
  bid <- liftIO $ flip runSqlPool pool $ insert b
  pure BookingDTO
    { bookingId = fromSqlKey bid
    , title     = bookingTitle b
    , startsAt  = bookingStartsAt b
    , endsAt    = bookingEndsAt b
    , status    = T.pack (show (bookingStatus b))
    , notes     = bookingNotes b
    }
  where
    parseStatus t =
      case readMaybe (T.unpack t) of
        Just s  -> s
        Nothing -> Confirmed

-- Packages
packageServer :: ServerT PackageAPI AppM
packageServer = listProducts :<|> createPurchase

listProducts :: AppM [PackageProductDTO]
listProducts = do
  Env pool _ <- ask
  ps <- liftIO $ flip runSqlPool pool $ selectList [PackageProductActive ==. True] [Asc PackageProductId]
  pure $ map toDTO ps
  where
    toDTO (Entity pid p) = PackageProductDTO
      { ppId         = fromSqlKey pid
      , ppName       = packageProductName p
      , ppService    = T.pack (show (packageProductServiceKind p))
      , ppUnitsKind  = T.pack (show (packageProductUnitsKind p))
      , ppUnitsQty   = packageProductUnitsQty p
      , ppPriceCents = packageProductPriceCents p
      }

createPurchase :: PackagePurchaseReq -> AppM NoContent
createPurchase req = do
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let buyer = toSqlKey (buyerId req)   :: Key Party
      prodK = toSqlKey (productId req) :: Key PackageProduct
  liftIO $ flip runSqlPool pool $ do
    mp <- get prodK
    case mp of
      Nothing -> pure ()
      Just p -> do
        let qty    = packageProductUnitsQty p
            priceC = packageProductPriceCents p
        _ <- insert PackagePurchase
              { packagePurchaseBuyerId        = buyer
              , packagePurchaseProductId      = prodK
              , packagePurchasePurchasedAt    = now
              , packagePurchasePriceCents     = priceC
              , packagePurchaseExpiresAt      = Nothing
              , packagePurchaseRemainingUnits = qty
              , packagePurchaseStatus         = "Active"
              }
        pure ()
  pure NoContent

-- Invoices
invoiceServer :: ServerT InvoiceAPI AppM
invoiceServer = listInvoices :<|> createInvoice

listInvoices :: AppM [InvoiceDTO]
listInvoices = do
  Env pool _ <- ask
  is <- liftIO $ flip runSqlPool pool $ selectList [] [Desc InvoiceId]
  pure $ map toDTO is
  where
    toDTO (Entity iid i) = InvoiceDTO
      { invId     = fromSqlKey iid
      , number    = invoiceNumber i
      , statusI   = T.pack (show (invoiceStatus i))
      , subtotalC = invoiceSubtotalCents i
      , taxC      = invoiceTaxCents i
      , totalC    = invoiceTotalCents i
      }

createInvoice :: CreateInvoiceReq -> AppM InvoiceDTO
createInvoice req = do
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let day = utctDay now
      cid = toSqlKey (ciCustomerId req) :: Key Party
      inv = Invoice
        { invoiceCustomerId    = cid
        , invoiceIssueDate     = day
        , invoiceDueDate       = day
        , invoiceNumber        = ciNumber req
        , invoiceStatus        = Draft
        , invoiceCurrency      = "USD"
        , invoiceSubtotalCents = ciSubtotalCents req
        , invoiceTaxCents      = ciTaxCents req
        , invoiceTotalCents    = ciTotalCents req
        , invoiceSriDocumentId = Nothing
        , invoiceNotes         = Nothing
        , invoiceCreatedAt     = now
        }
  iid <- liftIO $ flip runSqlPool pool $ insert inv
  pure InvoiceDTO
    { invId     = fromSqlKey iid
    , number    = invoiceNumber inv
    , statusI   = T.pack (show (invoiceStatus inv))
    , subtotalC = invoiceSubtotalCents inv
    , taxC      = invoiceTaxCents inv
    , totalC    = invoiceTotalCents inv
    }

-- Admin (temporary)
adminServer :: ServerT AdminAPI AppM
adminServer = seedHandler

seedHandler :: AppM NoContent
seedHandler = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool seedAll
  pure NoContent
