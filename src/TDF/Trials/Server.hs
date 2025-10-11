{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Trials.Server where

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)

import           Network.Wai                     (Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)

import           Database.Persist.Sql

import           TDF.Auth             (AuthedUser)
import           TDF.Models          (Party(..), PartyId, ResourceId, partyDisplayName)
import qualified TDF.Models          as Models
import           TDF.Trials.API
import           TDF.Trials.DTO
import           TDF.Trials.Models
import qualified TDF.Trials.Models      as Trials

type AppM = SqlPersistT IO

statusRequested, statusAssigned, statusScheduled :: Text
statusRequested = "Requested"
statusAssigned  = "Assigned"
statusScheduled = "Scheduled"

entityKeyInt :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record) => Key record -> Int
entityKeyInt = fromIntegral . fromSqlKey

intKey :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record) => Int -> Key record
intKey i = toSqlKey (fromIntegral i :: Int64)

maybeKey :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record) => Maybe Int -> Maybe (Key record)
maybeKey = fmap intKey

preferredSlotsFrom :: TrialRequest -> [PreferredSlot]
preferredSlotsFrom req =
  PreferredSlot (trialRequestPref1Start req) (trialRequestPref1End req)
    : catMaybes
        [ mkSlot (trialRequestPref2Start req) (trialRequestPref2End req)
        , mkSlot (trialRequestPref3Start req) (trialRequestPref3End req)
        ]
  where
    mkSlot (Just s) (Just e) = Just (PreferredSlot s e)
    mkSlot _        _        = Nothing

trialRequestToQueueItem
  :: Map.Map SubjectId Text
  -> Map.Map PartyId Party
  -> Entity TrialRequest
  -> TrialQueueItem
trialRequestToQueueItem subjectMap partyMap (Entity rid req) =
  TrialQueueItem
    { requestId   = entityKeyInt rid
    , studentId   = Just (entityKeyInt (trialRequestPartyId req))
    , studentName = fmap partyDisplayName (Map.lookup (trialRequestPartyId req) partyMap)
    , subjectId   = entityKeyInt (trialRequestSubjectId req)
    , subjectName = Map.lookup (trialRequestSubjectId req) subjectMap
    , status      = trialRequestStatus req
    , preferred   = preferredSlotsFrom req
    , createdAt   = Just (trialRequestCreatedAt req)
    , notes       = trialRequestNotes req
    }

trialRequestOut :: Key TrialRequest -> TrialRequest -> TrialRequestOut
trialRequestOut rid req =
  TrialRequestOut
    { requestId = entityKeyInt rid
    , status    = trialRequestStatus req
    }

publicTrialsServer :: ServerT PublicTrialsAPI AppM
publicTrialsServer =
  signupH
    :<|> interestH
    :<|> trialRequestCreateH
  where
    signupH :: SignupIn -> AppM SignupOut
    signupH SignupIn{..} = do
      now <- liftIO getCurrentTime
      let partyIdKey = intKey 0 -- placeholder, until a full signup flow is implemented
      _ <- insert $ LeadInterest
        { leadInterestPartyId   = partyIdKey
        , leadInterestInterestType = "signup"
        , leadInterestSubjectId = Nothing
        , leadInterestDetails   = Just (T.intercalate " | " (filter (not . T.null)
            [ firstName <> " " <> lastName
            , email
            , maybe "" id phone
            ]))
        , leadInterestSource    = "public_signup"
        , leadInterestDriveLink = Nothing
        , leadInterestStatus    = "Open"
        , leadInterestCreatedAt = now
        }
      pure (SignupOut True)

    interestH :: InterestIn -> AppM InterestOut
    interestH InterestIn{..} = do
      now <- liftIO getCurrentTime
      let partyIdKey = intKey 0
      let subjectKey = maybeKey subjectId
      key <- insert LeadInterest
        { leadInterestPartyId   = partyIdKey
        , leadInterestInterestType = interestType
        , leadInterestSubjectId = subjectKey
        , leadInterestDetails   = details
        , leadInterestSource    = "public_interest"
        , leadInterestDriveLink = driveLink
        , leadInterestStatus    = "Open"
        , leadInterestCreatedAt = now
        }
      pure (InterestOut (entityKeyInt key))

    trialRequestCreateH :: TrialRequestIn -> AppM TrialRequestOut
    trialRequestCreateH TrialRequestIn{..} = do
      now <- liftIO getCurrentTime
      case preferred of
        [] -> liftIO $ throwIO err400 { errBody = "Need at least one preferred slot" }
        (PreferredSlot firstStart firstEnd : rest) -> do
          let pref2 = listToMaybe rest
              pref3 = listToMaybe (drop 1 rest)
              (pref2Start, pref2End) = slotBounds pref2
              (pref3Start, pref3End) = slotBounds pref3
              partyKey = maybe (intKey 0) intKey partyId
              subjectKey = intKey subjectId
          rid <- insert TrialRequest
            { trialRequestPartyId           = partyKey
            , trialRequestSubjectId         = subjectKey
            , trialRequestPref1Start        = firstStart
            , trialRequestPref1End          = firstEnd
            , trialRequestPref2Start        = pref2Start
            , trialRequestPref2End          = pref2End
            , trialRequestPref3Start        = pref3Start
            , trialRequestPref3End          = pref3End
            , trialRequestNotes             = notes
            , trialRequestStatus            = statusRequested
            , trialRequestAssignedTeacherId = Nothing
            , trialRequestAssignedAt        = Nothing
            , trialRequestCreatedAt         = now
            }
          pure (TrialRequestOut (entityKeyInt rid) statusRequested)

    slotBounds :: Maybe PreferredSlot -> (Maybe UTCTime, Maybe UTCTime)
    slotBounds = maybe (Nothing, Nothing) $ \(PreferredSlot s e) -> (Just s, Just e)

privateTrialsServer :: ServerT PrivateTrialsAPI AppM
privateTrialsServer =
  queueH
    :<|> assignH
    :<|> scheduleH
    :<|> subjectsH
    :<|> packagesH
    :<|> purchaseH
    :<|> createClassH
    :<|> attendH
    :<|> commissionsH
  where
    queueH :: Maybe Int -> Maybe Text -> AppM [TrialQueueItem]
    queueH mSubject mStatus = do
      let filters = catMaybes
            [ (TrialRequestSubjectId ==.) . intKey <$> mSubject
            , (TrialRequestStatus ==.) . T.strip <$> mStatus
            ]
      requests <- selectList filters [Desc TrialRequestCreatedAt]
      let subjectIds = map (trialRequestSubjectId . entityVal) requests
          partyIds   = map (trialRequestPartyId . entityVal) requests
      subjects <- if null subjectIds
        then pure Map.empty
        else do
          entities <- selectList [SubjectId <-. subjectIds] []
          pure $ Map.fromList [ (entityKey e, Trials.subjectName (entityVal e)) | e <- entities ]
      parties <- if null partyIds
        then pure Map.empty
        else do
          entities <- selectList [Models.PartyId <-. partyIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- entities ]
      pure (map (trialRequestToQueueItem subjects parties) requests)

    assignH :: Int -> TrialAssignIn -> AppM TrialRequestOut
    assignH requestId TrialAssignIn{..} = do
      let rid = intKey requestId :: Key TrialRequest
          teacherKey = intKey teacherId :: PartyId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          update rid
            [ TrialRequestAssignedTeacherId =. Just teacherKey
            , TrialRequestAssignedAt        =. Just now
            , TrialRequestStatus            =. statusAssigned
            ]
          pure (trialRequestOut rid req { trialRequestStatus = statusAssigned })

    scheduleH :: TrialScheduleIn -> AppM TrialRequestOut
    scheduleH TrialScheduleIn{..} = do
      let rid       = intKey requestId :: Key TrialRequest
          teacherK  = intKey teacherId :: PartyId
          roomK     = intKey roomId    :: ResourceId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          let assignment = TrialAssignment
                { trialAssignmentRequestId = rid
                , trialAssignmentTeacherId = teacherK
                , trialAssignmentStartAt   = startAt
                , trialAssignmentEndAt     = endAt
                , trialAssignmentRoomId    = roomK
                , trialAssignmentBookingId = Nothing
                , trialAssignmentCreatedAt = now
                }
          _ <- upsert assignment
            [ TrialAssignmentTeacherId =. teacherK
            , TrialAssignmentStartAt   =. startAt
            , TrialAssignmentEndAt     =. endAt
            , TrialAssignmentRoomId    =. roomK
            ]
          update rid
            [ TrialRequestAssignedTeacherId =. Just teacherK
            , TrialRequestAssignedAt        =. Just now
            , TrialRequestStatus            =. statusScheduled
            ]
          pure (trialRequestOut rid req { trialRequestStatus = statusScheduled })

    subjectsH :: AppM [SubjectDTO]
    subjectsH = do
      entities <- selectList [SubjectActive ==. True] [Asc SubjectName]
      pure [ SubjectDTO (entityKeyInt sid) (Trials.subjectName subj)
           | Entity sid subj <- entities
           ]

    packagesH :: Maybe Int -> AppM [PackageDTO]
    packagesH mSubject = do
      let filters = [PackageCatalogActive ==. True] ++ maybe [] (\sid -> [PackageCatalogSubjectId ==. intKey sid]) mSubject
      entities <- selectList filters [Asc PackageCatalogName]
      pure [ PackageDTO
              { packageId   = entityKeyInt pid
              , name        = packageCatalogName pkg
              , hoursQty    = packageCatalogHoursQty pkg
              , priceCents  = packageCatalogPriceCents pkg
              , expiresDays = packageCatalogExpiresDays pkg
              }
           | Entity pid pkg <- entities
           ]

    purchaseH :: PurchaseIn -> AppM PurchaseOut
    purchaseH PurchaseIn{..} = do
      now <- liftIO getCurrentTime
      let studentKey = intKey studentId
          packageKey = intKey packageId
          sellerKey  = maybeKey sellerId
          commissionKey = maybeKey commissionedTeacherId
          trialKey   = maybeKey trialRequestId
          discount   = fromMaybe 0 discountCents
          tax        = fromMaybe 0 taxCents
          total      = priceCents - discount + tax
      pid <- insert ClassPackagePurchase
        { classPackagePurchaseStudentId            = studentKey
        , classPackagePurchasePackageId            = packageKey
        , classPackagePurchasePriceCents           = priceCents
        , classPackagePurchaseDiscountCents        = discount
        , classPackagePurchaseTaxCents             = tax
        , classPackagePurchaseTotalPaidCents       = total
        , classPackagePurchasePurchasedAt          = now
        , classPackagePurchaseSellerId             = sellerKey
        , classPackagePurchaseCommissionedTeacherId = commissionKey
        , classPackagePurchaseTrialRequestId       = trialKey
        , classPackagePurchaseStatus               = "Open"
        }
      pure (PurchaseOut (entityKeyInt pid))

    createClassH :: ClassSessionIn -> AppM ClassSessionOut
    createClassH ClassSessionIn{..} = do
      let studentKey = intKey studentId
          teacherKey = intKey teacherId
          subjectKey = intKey subjectId
          roomKey    = intKey roomId :: ResourceId
          bookingKey = maybeKey bookingId
          durationMinutes = floor (realToFrac (diffUTCTime endAt startAt) / 60 :: Double)
      sid <- insert ClassSession
        { classSessionStudentId       = studentKey
        , classSessionTeacherId       = teacherKey
        , classSessionSubjectId       = subjectKey
        , classSessionStartAt         = startAt
        , classSessionEndAt           = endAt
        , classSessionRoomId          = roomKey
        , classSessionBookingId       = bookingKey
        , classSessionAttended        = False
        , classSessionPurchaseId      = Nothing
        , classSessionConsumedMinutes = max 0 durationMinutes
        , classSessionNotes           = Nothing
        }
      pure (ClassSessionOut (entityKeyInt sid) (max 0 durationMinutes))

    attendH :: Int -> AttendIn -> AppM ClassSessionOut
    attendH classId AttendIn{..} = do
      let cid = intKey classId :: Key ClassSession
      mSession <- get cid
      case mSession of
        Nothing -> liftIO $ throwIO err404
        Just sess -> do
          let duration = classSessionConsumedMinutes sess
          update cid
            [ ClassSessionAttended =. attended
            , ClassSessionNotes    =. notes
            ]
          pure (ClassSessionOut (entityKeyInt cid) duration)

    commissionsH :: Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> AppM [CommissionDTO]
    commissionsH mFrom mTo mTeacher = do
      let baseFilters = catMaybes
            [ (CommissionRecognizedAt >=.) <$> mFrom
            , (CommissionRecognizedAt <=.) <$> mTo
            , (CommissionTeacherId ==.) . intKey <$> mTeacher
            ]
      entities <- selectList baseFilters [Desc CommissionRecognizedAt]
      pure [ CommissionDTO
              { teacherId  = entityKeyInt (commissionTeacherId commission)
              , amountCents = commissionAmountCents commission
              , basisCents  = commissionBasisCents commission
              , percent     = commissionPercent commission
              }
           | Entity _ commission <- entities
           ]


trialsServer :: ConnectionPool -> Server TrialsAPI
trialsServer pool =
  let trialsProxy = Proxy :: Proxy TrialsAPI
      ctxProxy    = Proxy :: Proxy '[AuthHandler Request AuthedUser]
      server      = publicTrialsServer :<|> authedPrivateServer
  in hoistServerWithContext trialsProxy ctxProxy nt server
  where
    nt :: AppM a -> Handler a
    nt x = liftIO (runSqlPool x pool)

    authedPrivateServer :: AuthedUser -> ServerT PrivateTrialsAPI AppM
    authedPrivateServer _ = privateTrialsServer
