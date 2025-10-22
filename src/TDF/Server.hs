{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.Server where

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Crypto.BCrypt (validatePassword)
import           Data.Int (Int64)
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe)
import           Data.Time (getCurrentTime, utctDay)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Text.Read (readMaybe)

import           Servant
import           Network.Wai (Request)
import qualified Data.ByteString.Lazy as BL
import           Servant.Server.Experimental.Auth (AuthHandler)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.API.Types (RolePayload(..))
import           TDF.DB
import           TDF.Models
import           TDF.DTO
import           TDF.Auth (AuthedUser(..), ModuleAccess(..), authContext, hasModuleAccess, moduleName, loadAuthedUser)
import           TDF.ServerAdmin (adminServer)
import           TDF.ServerExtra (bandsServer, inventoryServer, loadBandForParty, pipelinesServer, roomsServer, sessionsServer)
import           TDF.ServerFuture (futureServer)
import           TDF.Trials.API (TrialsAPI)
import           TDF.Trials.Server (trialsServer)

type AppM = ReaderT Env Handler

type CombinedAPI = TrialsAPI :<|> API

mkApp :: Env -> Application
mkApp env =
  let apiProxy = Proxy :: Proxy API
      combinedProxy = Proxy :: Proxy CombinedAPI
      ctxProxy = Proxy :: Proxy '[AuthHandler Request AuthedUser]
      ctx      = authContext env
      trials   = trialsServer (envPool env)
      apiSrv   = hoistServerWithContext apiProxy ctxProxy (nt env) server
  in serveWithContext combinedProxy ctx (trials :<|> apiSrv)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

server :: ServerT API AppM
server =
       health
  :<|> login
  :<|> protectedServer

protectedServer :: AuthedUser -> ServerT ProtectedAPI AppM
protectedServer user =
       partyServer user
  :<|> bookingServer user
  :<|> packageServer user
  :<|> invoiceServer user
  :<|> adminServer user
  :<|> inventoryServer user
  :<|> bandsServer user
  :<|> sessionsServer user
  :<|> pipelinesServer user
  :<|> roomsServer user
  :<|> futureServer

-- Health
health :: AppM TDF.API.HealthStatus
health = pure (HealthStatus "ok" "ok")

login :: LoginRequest -> AppM LoginResponse
login LoginRequest{..} = do
  Env pool _ <- ask
  result <- liftIO $ flip runSqlPool pool (runLogin username password)
  case result of
    Left msg  -> throwAuthError msg
    Right res -> pure res
  where
    throwAuthError msg = throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

runLogin :: Text -> Text -> SqlPersistT IO (Either Text LoginResponse)
runLogin uname pwd = do
  mCred <- getBy (UniqueCredentialUsername uname)
  case mCred of
    Nothing -> pure (Left invalidMsg)
    Just (Entity _ cred)
      | not (userCredentialActive cred) -> pure (Left "Account disabled")
      | otherwise ->
          if validatePassword (TE.encodeUtf8 (userCredentialPasswordHash cred)) (TE.encodeUtf8 pwd)
            then do
              token <- createSessionToken (userCredentialPartyId cred) uname
              mUser  <- loadAuthedUser token
              case mUser of
                Nothing    -> pure (Left "Failed to load user profile")
                Just user  -> pure (Right (toLoginResponse token user))
            else pure (Left invalidMsg)
  where
    invalidMsg = "Invalid username or password"

toLoginResponse :: Text -> AuthedUser -> LoginResponse
toLoginResponse token AuthedUser{..} = LoginResponse
  { token   = token
  , partyId = fromSqlKey auPartyId
  , roles   = auRoles
  , modules = map moduleName (Set.toList auModules)
  }

createSessionToken :: PartyId -> Text -> SqlPersistT IO Text
createSessionToken pid uname = do
  token <- liftIO (toText <$> nextRandom)
  let label = Just ("password-login:" <> uname)
  inserted <- insertUnique (ApiToken token pid label True)
  case inserted of
    Nothing -> createSessionToken pid uname
    Just _  -> pure token

-- Parties
partyServer :: AuthedUser -> ServerT PartyAPI AppM
partyServer user = listParties user :<|> createParty user :<|> partyById
  where
    partyById pid = getParty user pid :<|> updateParty user pid :<|> addRole user pid

listParties :: AuthedUser -> AppM [PartyDTO]
listParties user = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  entities <- liftIO $ flip runSqlPool pool $ selectList [] [Asc PartyId]
  pure (map toPartyDTO entities)

createParty :: AuthedUser -> PartyCreate -> AppM PartyDTO
createParty user req = do
  requireModule user ModuleCRM
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
  liftIO $ flip runSqlPool pool $ mapM_ (\role -> upsert
    (PartyRole pid role True)
    [PartyRoleActive =. True]) (fromMaybe [] (cRoles req))
  pure $ toPartyDTO (Entity pid p)

getParty :: AuthedUser -> Int64 -> AppM PartyDTO
getParty user pidI = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  mp <- liftIO $ flip runSqlPool pool $ getEntity pid
  case mp of
    Nothing -> throwError err404
    Just ent -> do
      bandDetails <- liftIO $ flip runSqlPool pool $ loadBandForParty (entityKey ent)
      pure (toPartyDTOWithBand bandDetails ent)

updateParty :: AuthedUser -> Int64 -> PartyUpdate -> AppM PartyDTO
updateParty user pidI req = do
  requireModule user ModuleCRM
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
  getParty user pidI

addRole :: AuthedUser -> Int64 -> RolePayload -> AppM NoContent
addRole user pidI (RolePayload roleTxt) = do
  requireModule user ModuleAdmin
  Env pool _ <- ask
  let pid  = toSqlKey pidI :: Key Party
      role = parseRole roleTxt
  liftIO $ flip runSqlPool pool $ void $ upsert
    (PartyRole pid role True)
    [ PartyRoleActive =. True ]
  pure NoContent
  where
    parseRole t =
      case readMaybe (T.unpack (T.strip t)) of
        Just r  -> r
        Nothing -> ReadOnly

-- Bookings
bookingServer :: AuthedUser -> ServerT BookingAPI AppM
bookingServer user = listBookings user :<|> createBooking user

listBookings :: AuthedUser -> AppM [BookingDTO]
listBookings user = do
  requireModule user ModuleScheduling
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

createBooking :: AuthedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  requireModule user ModuleScheduling
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
packageServer :: AuthedUser -> ServerT PackageAPI AppM
packageServer user = listProducts user :<|> createPurchase user

listProducts :: AuthedUser -> AppM [PackageProductDTO]
listProducts user = do
  requireModule user ModulePackages
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

createPurchase :: AuthedUser -> PackagePurchaseReq -> AppM NoContent
createPurchase user req = do
  requireModule user ModulePackages
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
invoiceServer :: AuthedUser -> ServerT InvoiceAPI AppM
invoiceServer user = listInvoices user :<|> createInvoice user

listInvoices :: AuthedUser -> AppM [InvoiceDTO]
listInvoices user = do
  requireModule user ModuleInvoicing
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

createInvoice :: AuthedUser -> CreateInvoiceReq -> AppM InvoiceDTO
createInvoice user req = do
  requireModule user ModuleInvoicing
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

requireModule :: AuthedUser -> ModuleAccess -> AppM ()
requireModule user moduleTag
  | hasModuleAccess moduleTag user = pure ()
  | otherwise = throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
  where
    msg = "Missing access to module: " <> moduleName moduleTag
