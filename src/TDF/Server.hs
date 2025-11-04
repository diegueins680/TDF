{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TDF.Server where

import           Control.Applicative ((<|>))
import           Control.Monad (forM, forM_, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.Int (Int64)
import           Data.List (find, foldl', nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import           Data.Time (Day, UTCTime, fromGregorian, getCurrentTime, toGregorian, utctDay)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Crypto.BCrypt (validatePassword)
import           Network.Wai (Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import           Web.PathPieces (fromPathPiece, toPathPiece)
import           Data.Proxy (Proxy (..))

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.API.Types (RolePayload(..))
import qualified TDF.API      as Api
import           TDF.Config (AppConfig(..))
import           TDF.DB
import           TDF.Models
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import           TDF.DTO
import           TDF.Auth (AuthedUser(..), ModuleAccess(..), authContext, hasModuleAccess, moduleName, loadAuthedUser)
import           TDF.Seed       (seedAll)
import           TDF.ServerAdmin (adminServer)
import           TDF.ServerExtra (bandsServer, inventoryServer, loadBandForParty, pipelinesServer, roomsServer, sessionsServer)
import           TDF.ServerFuture (futureServer)
import           TDF.Trials.API (TrialsAPI)
import           TDF.Trials.Server (trialsServer)
import qualified TDF.Meta as Meta
import           TDF.Version      (getVersionInfo)
import qualified TDF.Handlers.InputList as InputList

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
       versionServer
  :<|> health
  :<|> login
  :<|> metaServer
  :<|> seedTrigger
  :<|> inputListServer
  :<|> protectedServer

versionServer :: ServerT Api.VersionAPI AppM
versionServer = liftIO getVersionInfo

inputListServer :: ServerT Api.InputListPublicAPI AppM
inputListServer =
       listInventory
  :<|> seedInventory
  :<|> getSessionInputList
  :<|> seedHQ
  :<|> getSessionInputListPdf

listInventory :: AppM [Entity InputList.InventoryItem]
listInventory = do
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool InputList.listInventoryDB

seedInventory :: Maybe Text -> AppM NoContent
seedInventory rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool InputList.seedInventoryDB
  pure NoContent

seedHQ :: Maybe Text -> AppM NoContent
seedHQ rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool envPool (InputList.seedHQDB now)
  pure NoContent

requireSeedToken :: Maybe Text -> AppM ()
requireSeedToken rawToken = do
  Env{..} <- ask
  let encode = BL.fromStrict . TE.encodeUtf8
      missingHeader = throwError err401 { errBody = encode "Missing X-Seed-Token header" }
      disabled = throwError err403 { errBody = encode "Seeding endpoint disabled" }
      invalid = throwError err403 { errBody = encode "Invalid seed token" }
  secret <- maybe disabled pure (seedTriggerToken envConfig)
  token  <- maybe missingHeader (pure . T.strip) rawToken
  when (T.null token) missingHeader
  when (token /= secret) invalid
  pure ()

getSessionInputList :: Maybe Int -> Maybe Text -> AppM [Entity InputList.InputListEntry]
getSessionInputList mIndex mSessionId = do
  (_session, rows) <- resolveSessionInputData mIndex mSessionId
  pure rows

getSessionInputListPdf
  :: Maybe Int
  -> Maybe Text
  -> AppM (Headers '[Header "Content-Disposition" Text] BL.ByteString)
getSessionInputListPdf mIndex mSessionId = do
  (Entity _ session, rows) <- resolveSessionInputData mIndex mSessionId
  let title = fromMaybe (ME.sessionService session <> " session") (ME.sessionClientPartyRef session)
      latex = InputList.renderInputListLatex title rows
  pdfResult <- liftIO (InputList.generateInputListPdf latex)
  case pdfResult of
    Left errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
    Right pdf -> do
      let fileName    = InputList.sanitizeFileName title <> ".pdf"
          disposition = T.concat ["attachment; filename=\"", fileName, "\""]
      pure (addHeader disposition pdf)

resolveSessionInputData
  :: Maybe Int
  -> Maybe Text
  -> AppM (Entity ME.Session, [Entity InputList.InputListEntry])
resolveSessionInputData mIndex mSessionId = do
  Env{..} <- ask
  action <- case mSessionId of
    Just rawId ->
      case fromPathPiece rawId of
        Nothing     -> throwBadRequest "Invalid sessionId"
        Just keyVal -> pure (InputList.fetchSessionInputRowsByKey keyVal)
    Nothing -> do
      idx <- case mIndex of
        Nothing     -> pure 1
        Just n
          | n >= 1    -> pure n
          | otherwise -> throwBadRequest "index must be greater than or equal to 1"
      pure (InputList.fetchSessionInputRowsByIndex idx)
  result <- liftIO $ flip runSqlPool envPool action
  maybe (throwError err404) pure result

protectedServer :: AuthedUser -> ServerT ProtectedAPI AppM
protectedServer user =
       partyServer user
  :<|> bookingServer user
  :<|> packageServer user
  :<|> invoiceServer user
  :<|> receiptServer user
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

metaServer :: ServerT Meta.MetaAPI AppM
metaServer = hoistServer metaProxy lift Meta.metaServer
  where
    metaProxy = Proxy :: Proxy Meta.MetaAPI

seedTrigger :: Maybe Text -> AppM NoContent
seedTrigger rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool seedAll
  pure NoContent

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
  (bookings, resourceMap) <- liftIO $ flip runSqlPool pool $ do
    bs <- selectList [] [Desc BookingId]
    resMap <- loadBookingResourceMap (map entityKey bs)
    pure (bs, resMap)
  pure $ map (toDTO resourceMap) bookings
  where
    toDTO resMap (Entity bid b) = BookingDTO
      { bookingId   = fromSqlKey bid
      , title       = bookingTitle b
      , startsAt    = bookingStartsAt b
      , endsAt      = bookingEndsAt b
      , status      = T.pack (show (bookingStatus b))
      , notes       = bookingNotes b
      , partyId     = fmap fromSqlKey (bookingPartyId b)
      , serviceType = bookingServiceType b
      , resources   = Map.findWithDefault [] bid resMap
      }

createBooking :: AuthedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  now <- liftIO getCurrentTime

  let status'          = parseStatus (cbStatus req)
      serviceTypeClean = cleanText (cbServiceType req)
      partyKey         = fmap (toSqlKey . fromIntegral) (cbPartyId req)
      requestedRooms   = fromMaybe [] (cbResourceIds req)

  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean requestedRooms (cbStartsAt req) (cbEndsAt req)

  let bookingRecord = Booking
        { bookingTitle          = cbTitle req
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = partyKey
        , bookingServiceType    = serviceTypeClean
        , bookingStartsAt       = cbStartsAt req
        , bookingEndsAt         = cbEndsAt req
        , bookingStatus         = status'
        , bookingCreatedBy      = Nothing
        , bookingNotes          = cbNotes req
        , bookingCreatedAt      = now
        }

  (bid, resourceDtos) <- liftIO $ flip runSqlPool pool $ do
    bookingId <- insert bookingRecord
    let uniqueResources = nub resourceKeys
    forM_ (zip [0 :: Int ..] uniqueResources) $ \(idx, key) ->
      insert_ BookingResource
        { bookingResourceBookingId = bookingId
        , bookingResourceResourceId = key
        , bookingResourceRole = if idx == 0 then "primary" else "secondary"
        }
    resMap <- loadBookingResourceMap [bookingId]
    pure (bookingId, Map.findWithDefault [] bookingId resMap)

  pure BookingDTO
    { bookingId   = fromSqlKey bid
    , title       = bookingTitle bookingRecord
    , startsAt    = bookingStartsAt bookingRecord
    , endsAt      = bookingEndsAt bookingRecord
    , status      = T.pack (show (bookingStatus bookingRecord))
    , notes       = bookingNotes bookingRecord
    , partyId     = fmap fromSqlKey partyKey
    , serviceType = bookingServiceType bookingRecord
    , resources   = resourceDtos
    }
  where
    parseStatus t =
      case readMaybe (T.unpack t) of
        Just s  -> s
        Nothing -> Confirmed

    cleanText Nothing = Nothing
    cleanText (Just raw) =
      let trimmed = T.strip raw
      in if T.null trimmed then Nothing else Just trimmed


loadBookingResourceMap :: [Key Booking] -> SqlPersistT IO (Map.Map (Key Booking) [BookingResourceDTO])
loadBookingResourceMap [] = pure Map.empty
loadBookingResourceMap bookingIds = do
  bookingResources <- selectList [BookingResourceBookingId <-. bookingIds] []
  if null bookingResources
    then pure Map.empty
    else do
      let resourceIds = map (bookingResourceResourceId . entityVal) bookingResources
      resources <- selectList [ResourceId <-. resourceIds] []
      let resourceMap = Map.fromList [ (entityKey resEnt, resEnt) | resEnt <- resources ]
          accumulate acc (Entity _ br) =
            case Map.lookup (bookingResourceResourceId br) resourceMap of
              Nothing      -> acc
              Just resEnt  ->
                let bookingKey = bookingResourceBookingId br
                    dto = BookingResourceDTO
                      { brRoomId   = toPathPiece (bookingResourceResourceId br)
                      , brRoomName = resourceName (entityVal resEnt)
                      , brRole     = bookingResourceRole br
                      }
                in Map.insertWith (++) bookingKey [dto] acc
      pure (foldl' accumulate Map.empty bookingResources)

resolveResourcesForBooking :: Maybe Text -> [Text] -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
resolveResourcesForBooking service requested start end = do
  explicit <- resolveRequestedResources requested
  if not (null explicit)
    then pure explicit
    else defaultResourcesForService service start end

resolveRequestedResources :: [Text] -> SqlPersistT IO [Key Resource]
resolveRequestedResources ids = fmap catMaybes $ mapM lookupResource ids
  where
    lookupResource :: Text -> SqlPersistT IO (Maybe (Key Resource))
    lookupResource rid =
      case fromPathPiece rid of
        Nothing  -> pure Nothing
        Just key -> do
          mRes <- get key
          case mRes of
            Just res | resourceResourceType res == Room && resourceActive res -> pure (Just key)
            _        -> pure Nothing

defaultResourcesForService :: Maybe Text -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
defaultResourcesForService Nothing _ _ = pure []
defaultResourcesForService (Just service) start end = do
  let normalized = T.toLower (T.strip service)
  rooms <- selectList [ResourceResourceType ==. Room, ResourceActive ==. True] [Asc ResourceId]
  let findByName name = find (\(Entity _ room) -> T.toLower (resourceName room) == T.toLower name) rooms
      boothPredicate (Entity _ room) = "booth" `T.isInfixOf` T.toLower (resourceName room)
  case normalized of
    "band recording" ->
      pure $ map entityKey $ catMaybes (map findByName ["Live Room", "Control Room"])
    "vocal recording" ->
      let vocal = findByName "Vocal Booth" <|> find (\(Entity _ room) -> let lower = T.toLower (resourceName room) in "vocal" `T.isInfixOf` lower || "booth" `T.isInfixOf` lower) rooms
          control = findByName "Control Room"
      in pure $ map entityKey $ catMaybes [vocal, control]
    "band rehearsal" ->
      pure $ maybe [] (pure . entityKey) (findByName "Live Room")
    "dj booth rental" -> do
      let candidateNames = ["Booth 1","Booth 2","Booth A","Booth B","DJ Booth 1","DJ Booth 2"]
          nameMatches = mapMaybe findByName candidateNames
          boothMatches = filter boothPredicate rooms
          candidates = dedupeEntities (nameMatches ++ boothMatches)
      pickBooth candidates
    _ -> pure []
  where
    pickBooth [] = pure []
    pickBooth (Entity key room : rest) = do
      available <- isResourceAvailableDB key start end
      if available
        then pure [key]
        else do
          remaining <- pickBooth rest
          pure (if null remaining then [key] else remaining)

dedupeEntities :: [Entity Resource] -> [Entity Resource]
dedupeEntities = go Set.empty []
  where
    go _ acc [] = reverse acc
    go seen acc (e:es) =
      let key = entityKey e
      in if Set.member key seen
           then go seen acc es
           else go (Set.insert key seen) (e:acc) es

isResourceAvailableDB :: Key Resource -> UTCTime -> UTCTime -> SqlPersistT IO Bool
isResourceAvailableDB resourceKey start end = do
  bookingResources <- selectList [BookingResourceResourceId ==. resourceKey] []
  let bookingIds = map (bookingResourceBookingId . entityVal) bookingResources
  if null bookingIds
    then pure True
    else do
      bookings <- selectList [BookingId <-. bookingIds] []
      let activeBookings = filter (\(Entity _ b) -> bookingStatus b `notElem` [Cancelled, NoShow]) bookings
      pure $ all (noOverlap . entityVal) activeBookings
  where
    noOverlap booking =
      bookingEndsAt booking <= start || bookingStartsAt booking >= end

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
  liftIO $ flip runSqlPool pool $ do
    invoices <- selectList [] [Desc InvoiceId]
    let invoiceIds = map entityKey invoices
    lineEntities <-
      if null invoiceIds
        then pure []
        else selectList [InvoiceLineInvoiceId <-. invoiceIds] [Asc InvoiceLineId]
    receiptEntities <-
      if null invoiceIds
        then pure []
        else selectList [ReceiptInvoiceId <-. invoiceIds] []
    let lineMap = foldr (\ent@(Entity _ line) acc ->
                      Map.insertWith (++) (invoiceLineInvoiceId line) [ent] acc)
                    Map.empty
                    lineEntities
        receiptMap = Map.fromList
          [ (receiptInvoiceId rec, entityKey ent)
          | ent@(Entity _ rec) <- receiptEntities
          ]
    pure
      [ invoiceToDTO invEnt
          (Map.findWithDefault [] (entityKey invEnt) lineMap)
          (Map.lookup (entityKey invEnt) receiptMap)
      | invEnt <- invoices
      ]

createInvoice :: AuthedUser -> CreateInvoiceReq -> AppM InvoiceDTO
createInvoice user CreateInvoiceReq{..} = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  when (null ciLineItems) $ throwBadRequest "Invoice requires at least one line item"
  preparedLines <- case traverse prepareLine ciLineItems of
    Left msg   -> throwBadRequest msg
    Right vals -> pure vals
  now <- liftIO getCurrentTime
  let day      = utctDay now
      cid      = toSqlKey ciCustomerId :: Key Party
      currency = normalizeCurrency ciCurrency
      notes    = normalizeOptionalText ciNotes
      number   = normalizeOptionalText ciNumber
      subtotal = sum (map plSubtotal preparedLines)
      taxTotal = sum (map plTax preparedLines)
      grand    = sum (map plTotal preparedLines)
      invoiceRecord = Invoice
        { invoiceCustomerId    = cid
        , invoiceIssueDate     = day
        , invoiceDueDate       = day
        , invoiceNumber        = number
        , invoiceStatus        = Draft
        , invoiceCurrency      = currency
        , invoiceSubtotalCents = subtotal
        , invoiceTaxCents      = taxTotal
        , invoiceTotalCents    = grand
        , invoiceSriDocumentId = Nothing
        , invoiceNotes         = notes
        , invoiceCreatedAt     = now
        }
  (invoiceEnt, lineEntities, maybeReceiptKey) <- liftIO $ flip runSqlPool pool $ do
    iid <- insert invoiceRecord
    let invEntity = Entity iid invoiceRecord
    invoiceLines <- forM preparedLines $ \pl -> do
      let line = invoiceLineFromPrepared iid pl
      lid <- insert line
      pure (Entity lid line)
    receiptKey <-
      if fromMaybe False ciGenerateReceipt
        then do
          (receiptEnt, _) <- issueReceipt now Nothing Nothing notes Nothing invEntity invoiceLines
          pure (Just (entityKey receiptEnt))
        else pure Nothing
    pure (invEntity, invoiceLines, receiptKey)
  pure $ invoiceToDTO invoiceEnt lineEntities maybeReceiptKey

-- Receipts
receiptServer :: AuthedUser -> ServerT ReceiptAPI AppM
receiptServer user = listReceipts user :<|> createReceipt user :<|> getReceipt user

listReceipts :: AuthedUser -> AppM [ReceiptDTO]
listReceipts user = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    receipts <- selectList [] [Desc ReceiptId]
    let receiptIds = map entityKey receipts
    lineEntities <-
      if null receiptIds
        then pure []
        else selectList [ReceiptLineReceiptId <-. receiptIds] [Asc ReceiptLineId]
    let lineMap = foldr (\ent@(Entity _ line) acc ->
                      Map.insertWith (++) (receiptLineReceiptId line) [ent] acc)
                    Map.empty
                    lineEntities
    pure
      [ receiptToDTO recEnt (Map.findWithDefault [] (entityKey recEnt) lineMap)
      | recEnt <- receipts
      ]

createReceipt :: AuthedUser -> CreateReceiptReq -> AppM ReceiptDTO
createReceipt user CreateReceiptReq{..} = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let iid = toSqlKey crInvoiceId :: Key Invoice
  result <- liftIO $ flip runSqlPool pool $ do
    mInvoice <- getEntity iid
    case mInvoice of
      Nothing      -> pure (Left "invoice-not-found")
      Just invEnt -> do
        existing <- selectFirst [ReceiptInvoiceId ==. iid] []
        case existing of
          Just receiptEnt -> do
            lines <- selectList [ReceiptLineReceiptId ==. entityKey receiptEnt] [Asc ReceiptLineId]
            pure (Right (receiptToDTO receiptEnt lines))
          Nothing -> do
            invoiceLines <- selectList [InvoiceLineInvoiceId ==. iid] [Asc InvoiceLineId]
            if null invoiceLines
              then pure (Left "invoice-empty")
              else do
                (receiptEnt, receiptLines) <-
                  issueReceipt now (normalizeOptionalText crBuyerName) (normalizeOptionalText crBuyerEmail)
                               (normalizeOptionalText crNotes) (normalizeOptionalText crCurrency)
                               invEnt invoiceLines
                pure (Right (receiptToDTO receiptEnt receiptLines))
  case result of
    Left "invoice-not-found" -> throwError err404 { errBody = BL.fromStrict (TE.encodeUtf8 "Invoice not found") }
    Left "invoice-empty"     -> throwBadRequest "Invoice has no line items to receipt"
    Left otherMsg             -> throwBadRequest otherMsg
    Right dto                 -> pure dto

getReceipt :: AuthedUser -> Int64 -> AppM ReceiptDTO
getReceipt user ridParam = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  let rid = toSqlKey ridParam :: Key Receipt
  result <- liftIO $ flip runSqlPool pool $ do
    mReceipt <- getEntity rid
    case mReceipt of
      Nothing -> pure Nothing
      Just rec -> do
        lines <- selectList [ReceiptLineReceiptId ==. rid] [Asc ReceiptLineId]
        pure (Just (receiptToDTO rec lines))
  maybe (throwError err404) pure result

data PreparedLine = PreparedLine
  { plDescription       :: Text
  , plQuantity          :: Int
  , plUnitCents         :: Int
  , plTaxBps            :: Int
  , plServiceOrderId    :: Maybe (Key ServiceOrder)
  , plPackagePurchaseId :: Maybe (Key PackagePurchase)
  , plSubtotal          :: Int
  , plTax               :: Int
  , plTotal             :: Int
  }

prepareLine :: CreateInvoiceLineReq -> Either Text PreparedLine
prepareLine CreateInvoiceLineReq{..} = do
  let desc = T.strip cilDescription
  if T.null desc then Left "Line item description is required" else pure ()
  when (cilQuantity <= 0) $ Left "Line item quantity must be greater than zero"
  when (cilUnitCents < 0) $ Left "Line item unit amount must be zero or greater"
  let taxBpsVal = fromMaybe 0 cilTaxBps
  when (taxBpsVal < 0) $ Left "Line item tax basis points must be zero or greater"
  let subtotal = cilQuantity * cilUnitCents
      tax      = (subtotal * taxBpsVal) `div` 10000
      total    = subtotal + tax
      serviceOrderKey = (toSqlKey <$> cilServiceOrderId) :: Maybe (Key ServiceOrder)
      packagePurchaseKey = (toSqlKey <$> cilPackagePurchaseId) :: Maybe (Key PackagePurchase)
  pure PreparedLine
    { plDescription       = desc
    , plQuantity          = cilQuantity
    , plUnitCents         = cilUnitCents
    , plTaxBps            = taxBpsVal
    , plServiceOrderId    = serviceOrderKey
    , plPackagePurchaseId = packagePurchaseKey
    , plSubtotal          = subtotal
    , plTax               = tax
    , plTotal             = total
    }

invoiceLineFromPrepared :: Key Invoice -> PreparedLine -> InvoiceLine
invoiceLineFromPrepared iid PreparedLine{..} = InvoiceLine
  { invoiceLineInvoiceId         = iid
  , invoiceLineServiceOrderId    = plServiceOrderId
  , invoiceLinePackagePurchaseId = plPackagePurchaseId
  , invoiceLineDescription       = plDescription
  , invoiceLineQuantity          = plQuantity
  , invoiceLineUnitCents         = plUnitCents
  , invoiceLineTaxBps            = plTaxBps
  , invoiceLineTotalCents        = plTotal
  }

normalizeCurrency :: Maybe Text -> Text
normalizeCurrency mCur =
  case fmap T.strip mCur of
    Just cur | not (T.null cur) -> T.toUpper cur
    _                           -> "USD"

normalizeOptionalText :: Maybe Text -> Maybe Text
normalizeOptionalText =
  let clean t =
        let trimmed = T.strip t
        in if T.null trimmed then Nothing else Just trimmed
  in (>>= clean)

invoiceToDTO :: Entity Invoice -> [Entity InvoiceLine] -> Maybe (Key Receipt) -> InvoiceDTO
invoiceToDTO (Entity iid inv) lines mReceiptKey = InvoiceDTO
  { invId      = fromSqlKey iid
  , number     = invoiceNumber inv
  , statusI    = T.pack (show (invoiceStatus inv))
  , subtotalC  = invoiceSubtotalCents inv
  , taxC       = invoiceTaxCents inv
  , totalC     = invoiceTotalCents inv
  , currency   = invoiceCurrency inv
  , customerId = Just (fromSqlKey (invoiceCustomerId inv))
  , notes      = invoiceNotes inv
  , receiptId  = fmap fromSqlKey mReceiptKey
  , lineItems  = map invoiceLineToDTO lines
  }

invoiceLineToDTO :: Entity InvoiceLine -> InvoiceLineDTO
invoiceLineToDTO (Entity lid line) = InvoiceLineDTO
  { lineId            = fromSqlKey lid
  , description       = invoiceLineDescription line
  , quantity          = invoiceLineQuantity line
  , unitCents         = invoiceLineUnitCents line
  , taxBps            = invoiceLineTaxBps line
  , totalCents        = invoiceLineTotalCents line
  , serviceOrderId    = fromSqlKey <$> invoiceLineServiceOrderId line
  , packagePurchaseId = fromSqlKey <$> invoiceLinePackagePurchaseId line
  }

receiptToDTO :: Entity Receipt -> [Entity ReceiptLine] -> ReceiptDTO
receiptToDTO (Entity rid rec) lines = ReceiptDTO
  { receiptId     = fromSqlKey rid
  , receiptNumber = M.receiptNumber rec
  , issuedAt      = M.receiptIssuedAt rec
  , issueDate     = M.receiptIssueDate rec
  , buyerName     = M.receiptBuyerName rec
  , buyerEmail    = M.receiptBuyerEmail rec
  , currency      = M.receiptCurrency rec
  , subtotalCents = M.receiptSubtotalCents rec
  , taxCents      = M.receiptTaxCents rec
  , totalCents    = M.receiptTotalCents rec
  , notes         = M.receiptNotes rec
  , invoiceId     = fromSqlKey (M.receiptInvoiceId rec)
  , lineItems     = map receiptLineToDTO lines
  }

receiptLineToDTO :: Entity ReceiptLine -> ReceiptLineDTO
receiptLineToDTO (Entity lid line) = ReceiptLineDTO
  { receiptLineId = fromSqlKey lid
  , rlDescription = receiptLineDescription line
  , rlQuantity    = receiptLineQuantity line
  , rlUnitCents   = receiptLineUnitCents line
  , rlTaxBps      = receiptLineTaxBps line
  , rlTotalCents  = receiptLineTotalCents line
  }

issueReceipt
  :: UTCTime
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Entity Invoice
  -> [Entity InvoiceLine]
  -> SqlPersistT IO (Entity Receipt, [Entity ReceiptLine])
issueReceipt now mBuyerName mBuyerEmail mNotes mCurrency (Entity iid inv) lineEntities = do
  let customerId = invoiceCustomerId inv
  party <- get customerId
  let defaultName  = maybe "Cliente" partyDisplayName party
      defaultEmail = party >>= partyPrimaryEmail
      buyerName    = fromMaybe defaultName mBuyerName
      buyerEmail   = mBuyerEmail <|> defaultEmail
      currency     = maybe (invoiceCurrency inv) (normalizeCurrency . Just) mCurrency
      notes        = mNotes <|> invoiceNotes inv
      calcTotals (Entity _ line) =
        let lineSubtotal = invoiceLineQuantity line * invoiceLineUnitCents line
            lineTotal    = invoiceLineTotalCents line
        in (lineSubtotal, lineTotal - lineSubtotal, lineTotal)
      subtotals = [ s | ent <- lineEntities, let (s, _, _) = calcTotals ent ]
      taxPieces = [ t | ent <- lineEntities, let (_, t, _) = calcTotals ent ]
      totals    = [ tot | ent <- lineEntities, let (_, _, tot) = calcTotals ent ]
      subtotal  = sum subtotals
      taxTotal  = sum taxPieces
      total     = sum totals
  number <- generateReceiptNumber (utctDay now)
  let receiptRecord = Receipt
        { receiptInvoiceId    = iid
        , receiptNumber       = number
        , receiptIssueDate    = utctDay now
        , receiptIssuedAt     = now
        , receiptBuyerPartyId = Just customerId
        , receiptBuyerName    = buyerName
        , receiptBuyerEmail   = buyerEmail
        , receiptCurrency     = currency
        , receiptSubtotalCents = subtotal
        , receiptTaxCents     = taxTotal
        , receiptTotalCents   = total
        , receiptNotes        = notes
        , receiptCreatedAt    = now
        }
  rid <- insert receiptRecord
  receiptLines <- forM lineEntities $ \(Entity _ line) -> do
    let lineRecord = ReceiptLine
          { receiptLineReceiptId = rid
          , receiptLineDescription = invoiceLineDescription line
          , receiptLineQuantity    = invoiceLineQuantity line
          , receiptLineUnitCents   = invoiceLineUnitCents line
          , receiptLineTaxBps      = Just (invoiceLineTaxBps line)
          , receiptLineTotalCents  = invoiceLineTotalCents line
          }
    rlId <- insert lineRecord
    pure (Entity rlId lineRecord)
  pure (Entity rid receiptRecord, receiptLines)

generateReceiptNumber :: Day -> SqlPersistT IO Text
generateReceiptNumber day = do
  let (year, _, _) = toGregorian day
      start = fromGregorian year 1 1
      next  = fromGregorian (year + 1) 1 1
  countForYear <- count [ReceiptIssueDate >=. start, ReceiptIssueDate <. next]
  let sequenceNumber = countForYear + 1
  pure (T.pack (printf "R-%04d-%04d" year sequenceNumber))

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }


requireModule :: AuthedUser -> ModuleAccess -> AppM ()
requireModule user moduleTag
  | hasModuleAccess moduleTag user = pure ()
  | otherwise = throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
  where
    msg = "Missing access to module: " <> moduleName moduleTag
