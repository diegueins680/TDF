{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.Server where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Data.Time (getCurrentTime, UTCTime, Day, utctDay)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Text.Read (readMaybe)

import           Servant
import           Servant.API.Experimental.Auth (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import           Network.Wai (Application, Request, requestHeaders)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.DB
import           TDF.Models
import           TDF.DTO
import           TDF.Seed (seedAll)

type AppM = ReaderT Env Handler

data AuthenticatedUser = AuthenticatedUser
  { auRole :: RoleEnum
  }

type instance AuthServerData (AuthProtect "role-auth") = AuthenticatedUser

mkApp :: Env -> Application
mkApp env =
  let ctx = authContext env
      proxy = Proxy :: Proxy API
      ctxProxy = Proxy :: Proxy '[AuthHandler Request AuthenticatedUser]
  in serveWithContext proxy ctx (hoistServerWithContext proxy ctxProxy (nt env) server)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

authContext :: Env -> Context '[AuthHandler Request AuthenticatedUser]
authContext env = authHandler env :. EmptyContext

authHandler :: Env -> AuthHandler Request AuthenticatedUser
authHandler _ = mkAuthHandler $ \req -> do
  roleHeader <- maybe (throwError err401 { errBody = "Missing X-Role header" }) pure (lookup "X-Role" (requestHeaders req))
  roleText <- either (const (throwError err401 { errBody = "Invalid role encoding" })) pure (TE.decodeUtf8' roleHeader)
  roleEnum <- maybe (throwError err401 { errBody = "Unknown role" }) pure (readMaybe (T.unpack roleText))
  pure AuthenticatedUser { auRole = roleEnum }

authorize :: [RoleEnum] -> AuthenticatedUser -> AppM ()
authorize allowed user =
  unless (auRole user `elem` allowed) $
    throwError err403 { errBody = LBS.pack "Forbidden: insufficient role" }

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

partyServer :: ServerT PartyAPI AppM
partyServer user =
       listParties user
  :<|> createParty user
  :<|> partyById user
  where
    partyById u pid =
           getParty u pid
      :<|> updateParty u pid
      :<|> addRole u pid

listParties :: AuthenticatedUser -> AppM [PartyDTO]
listParties user = do
  authorize [Admin, Manager, Reception, ReadOnly] user
  listPartiesAction

listPartiesAction :: AppM [PartyDTO]
listPartiesAction = do
  Env pool _ <- ask
  entities <- liftIO $ flip runSqlPool pool $ selectList [] [Asc PartyId]
  pure (map toPartyDTO entities)

createParty :: AuthenticatedUser -> PartyCreate -> AppM PartyDTO
createParty user req = do
  authorize [Admin, Manager, Reception] user
  createPartyAction req

createPartyAction :: PartyCreate -> AppM PartyDTO
createPartyAction req = do
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

getParty :: AuthenticatedUser -> Int64 -> AppM PartyDTO
getParty user pidI = do
  authorize [Admin, Manager, Reception, ReadOnly] user
  getPartyAction pidI

getPartyAction :: Int64 -> AppM PartyDTO
getPartyAction pidI = do
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  mp <- liftIO $ flip runSqlPool pool $ getEntity pid
  case mp of
    Nothing -> throwError err404
    Just ent -> pure (toPartyDTO ent)

updateParty :: AuthenticatedUser -> Int64 -> PartyUpdate -> AppM PartyDTO
updateParty user pidI req = do
  authorize [Admin, Manager, Reception] user
  updatePartyAction pidI req

updatePartyAction :: Int64 -> PartyUpdate -> AppM PartyDTO
updatePartyAction pidI req = do
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
  getPartyAction pidI

addRole :: AuthenticatedUser -> Int64 -> Text -> AppM NoContent
addRole user pidI roleTxt = do
  authorize [Admin, Manager] user
  addRoleAction pidI roleTxt

addRoleAction :: Int64 -> Text -> AppM NoContent
addRoleAction pidI roleTxt = do
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

bookingServer :: ServerT BookingAPI AppM
bookingServer user = listBookings user :<|> createBooking user

listBookings :: AuthenticatedUser -> AppM [BookingDTO]
listBookings user = do
  authorize [Admin, Manager, Reception] user
  listBookingsAction

listBookingsAction :: AppM [BookingDTO]
listBookingsAction = do
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

createBooking :: AuthenticatedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  authorize [Admin, Manager, Reception] user
  createBookingAction req

createBookingAction :: CreateBookingReq -> AppM BookingDTO
createBookingAction req = do
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

packageServer :: ServerT PackageAPI AppM
packageServer user = listProducts user :<|> createPurchase user

listProducts :: AuthenticatedUser -> AppM [PackageProductDTO]
listProducts user = do
  authorize [Admin, Manager, Reception, Accounting] user
  listProductsAction

listProductsAction :: AppM [PackageProductDTO]
listProductsAction = do
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

createPurchase :: AuthenticatedUser -> PackagePurchaseReq -> AppM NoContent
createPurchase user req = do
  authorize [Admin, Manager, Reception] user
  createPurchaseAction req

createPurchaseAction :: PackagePurchaseReq -> AppM NoContent
createPurchaseAction req = do
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

invoiceServer :: ServerT InvoiceAPI AppM
invoiceServer user = listInvoices user :<|> createInvoice user

listInvoices :: AuthenticatedUser -> AppM [InvoiceDTO]
listInvoices user = do
  authorize [Admin, Manager, Accounting] user
  listInvoicesAction

listInvoicesAction :: AppM [InvoiceDTO]
listInvoicesAction = do
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

createInvoice :: AuthenticatedUser -> CreateInvoiceReq -> AppM InvoiceDTO
createInvoice user req = do
  authorize [Admin, Accounting] user
  createInvoiceAction req

createInvoiceAction :: CreateInvoiceReq -> AppM InvoiceDTO
createInvoiceAction req = do
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

adminServer :: ServerT AdminAPI AppM
adminServer user = seedHandler user

seedHandler :: AuthenticatedUser -> AppM NoContent
seedHandler user = do
  authorize [Admin] user
  seedHandlerAction

seedHandlerAction :: AppM NoContent
seedHandlerAction = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool seedAll
  pure NoContent
