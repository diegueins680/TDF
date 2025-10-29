{-# LANGUAGE OverloadedStrings #-}
module TDF.Seed where

import           Control.Monad          (forM, forM_, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time              (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import           TDF.Models
import           TDF.ModelsExtra        (DropdownOption(..))
import qualified TDF.ModelsExtra       as ME
import qualified TDF.Trials.Models     as Trials
import           TDF.Pipelines          (canonicalStage, defaultStage)

-- Seed data from Diego's YAML (normalized)
seedAll :: SqlPersistT IO ()
seedAll = do
  now <- liftIO getCurrentTime

  -- Parties: Artists & Teachers
  let artists =
        [ ("Arkabuz", Nothing)
        , ("El Bloque", Nothing)
        , ("Skanka Fe", Nothing)
        , ("Quimika Soul", Nothing)
        , ("Juano Ledesma", Just "Juan Ledesma")
        ]
  mapM_ (\(disp, mlegal) -> do
           _ <- ensurePartyRecord now disp mlegal
           pure ()
        ) artists

  let teachers =
        [ ("César Galarza", Nothing)
        , ("Fabricio Alomía", Nothing)
        , ("Juan Ledesma", Nothing)
        ]
  teacherPairs <- forM teachers $ \(disp, mlegal) -> do
    pid <- ensurePartyRecord now disp mlegal
    _ <- upsert (PartyRole pid Teacher True)
      [ PartyRoleActive =. True ]
    pure (disp, pid)

  -- Service Catalog
  let svc name kind pm rate tax = ServiceCatalog name kind pm rate tax True
  _ <- insertUnique (svc "Recording" Recording Hourly Nothing (Just 1200))
  _ <- insertUnique (svc "Mixing" Mixing PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Mastering" Mastering PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Rehearsal" Rehearsal Hourly (Just (15*100)) (Just 1200))
  _ <- insertUnique (svc "Classes" Classes Package Nothing (Just 1200))
  _ <- insertUnique (svc "Event Production" EventProduction Quote Nothing (Just 1200))

  -- Pipelines: seed sample cards for Mixing/Mastering
  let pipelineSeeds =
        [ (Mixing, "Arkabuz - Single A", Just "Arkabuz", Just "Brief", 10)
        , (Mixing, "Quimika - EP", Just "Quimika Soul", Just "Prep", 20)
        , (Mastering, "Skanka Fe - LP", Just "Skanka Fe", Just "v1", 10)
        , (Mastering, "El Bloque - Single", Just "El Bloque", Just "Approved", 20)
        ]
      ensurePipelineCard
        :: (ServiceKind, Text, Maybe Text, Maybe Text, Int)
        -> SqlPersistT IO ()
      ensurePipelineCard (kind, titleTxt, artistTxt, stageTxt, sortOrder) = do
        existing <- selectFirst
          [ ME.PipelineCardServiceKind ==. kind
          , ME.PipelineCardTitle ==. titleTxt
          ]
          []
        case existing of
          Just _  -> pure ()
          Nothing -> do
            let stageValue = maybe (defaultStage kind) id (stageTxt >>= canonicalStage kind)
            _ <- insert ME.PipelineCard
                  { ME.pipelineCardServiceKind = kind
                  , ME.pipelineCardTitle       = titleTxt
                  , ME.pipelineCardArtist      = artistTxt
                  , ME.pipelineCardStage       = stageValue
                  , ME.pipelineCardSortOrder   = sortOrder
                  , ME.pipelineCardNotes       = Nothing
                  , ME.pipelineCardCreatedAt   = now
                  , ME.pipelineCardUpdatedAt   = now
                  }
            pure ()
  mapM_ ensurePipelineCard pipelineSeeds

  -- Package Product: Guitar 24h
  _ <- insertUnique $ PackageProduct
        { packageProductName = "Guitar 24h"
        , packageProductServiceKind = Classes
        , packageProductUnitsKind = Hours
        , packageProductUnitsQty = 24
        , packageProductPriceCents = 500 * 100
        , packageProductExpiresDays = Just 120
        , packageProductTransferable = False
        , packageProductRefundPolicy = CreditOnly
        , packageProductActive = True
        }

  -- Resources (rooms)
  let rooms = ["Booth A","Booth B","Booth C","Booth D","Live Room","Control Room","Synth Room","Studio A","Studio B","Rehearsal 1","Classroom"]
  roomPairs <- forM rooms $ \r -> do
    existing <- selectFirst [ResourceName ==. r] []
    case existing of
      Just (Entity rid _) -> pure (r, rid)
      Nothing -> do
        rid <- insert $ Resource r (slugify r) Room Nothing True
        pure (r, rid)

  -- Subjects and room availability preferences
  let subjectSeeds =
        [ ("DJ", True, ["Classroom","Studio B"])
        , ("Producción Musical", True, ["Studio B","Control Room"])
        , ("Grabación", True, ["Studio A","Control Room"])
        ]
  subjectPairs <- forM subjectSeeds $ \(subjectName, isActive, roomNames) -> do
    sid <- ensureSubjectRecord subjectName isActive
    forM_ (zip [1 :: Int ..] roomNames) $ \(priority, roomName) -> do
      case lookup roomName roomPairs of
        Nothing     -> pure ()
        Just roomId -> ensureSubjectRoomPref sid roomId priority
    pure (subjectName, sid)

  let teacherSubjectSeeds =
        [ ("César Galarza", "DJ")
        , ("César Galarza", "Grabación")
        , ("Fabricio Alomía", "Producción Musical")
        , ("Juan Ledesma", "DJ")
        ]
  forM_ teacherSubjectSeeds $ \(teacherName, subjectName) ->
    case (lookup teacherName teacherPairs, lookup subjectName subjectPairs) of
      (Just teacherId, Just subjectId) -> ensureTeacherSubjectLink teacherId subjectId
      _                                -> pure ()

  -- Publish sample availability windows (45 minutes each)
  let minutes :: Int -> NominalDiffTime
      minutes m = realToFrac (m * 60)
      addMinutes m = addUTCTime (minutes m)
      availabilitySeeds =
        [ ("César Galarza", "DJ", "Classroom", 24*60 + 540)           -- mañana 09:00
        , ("César Galarza", "DJ", "Classroom", 24*60 + 600)           -- mañana 10:00
        , ("Fabricio Alomía", "Producción Musical", "Studio B", 36*60 + 600)
        , ("Juan Ledesma", "Grabación", "Studio A", 48*60 + 480)
        ]
  forM_ availabilitySeeds $ \(teacherName, subjectName, roomName, startMinutes) ->
    case ( lookup teacherName teacherPairs
         , lookup subjectName subjectPairs
         , lookup roomName roomPairs
         ) of
      (Just teacherId, Just subjectId, Just roomId) -> do
        let slotStart = addMinutes startMinutes now
            slotEnd   = addMinutes (startMinutes + 45) now
        ensureTeacherAvailabilitySlot now teacherId subjectId roomId slotStart slotEnd
      _ -> pure ()

  -- Staff accounts + API tokens for authentication examples
  let staffAccounts =
        [ ("TDF Admin", Just "TDF Admin", Admin, "admin-token", "admin", "password123")
        , ("Front Desk Manager", Nothing, Manager, "manager-token", "manager", "password123")
        , ("Reception", Nothing, Reception, "reception-token", "reception", "password123")
        , ("Accounting", Nothing, Accounting, "accounting-token", "accounting", "password123")
        , ("Scheduling", Nothing, Engineer, "scheduling-token", "scheduling", "password123")
        , ("Packages", Nothing, Customer, "packages-token", "packages", "password123")
        ]
  mapM_ (\(disp, mlegal, role, token, uname, pwd) -> do
           _ <- ensureStaff now disp mlegal role token uname pwd
           pure ()
        ) staffAccounts

  -- Dropdown options for admin-managed metadata
  let dropdowns =
        [ ("band-role", "Singer", Nothing, Just 1)
        , ("band-role", "Bassist", Nothing, Just 2)
        , ("band-role", "Guitar Player", Nothing, Just 3)
        , ("band-role", "Drummer", Nothing, Just 4)
        , ("band-genre", "Rock", Nothing, Just 1)
        , ("band-genre", "Pop", Nothing, Just 2)
        , ("band-genre", "Jazz", Nothing, Just 3)
        , ("band-genre", "Metal", Nothing, Just 4)
        , ("band-genre", "Reggae", Nothing, Just 5)
        ]
  mapM_ (ensureDropdownOption now) dropdowns

  pure ()

slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-"

ensureStaff :: UTCTime -> Text -> Maybe Text -> RoleEnum -> Text -> Text -> Text -> SqlPersistT IO (Key Party)
ensureStaff now name mlegal role token uname pwd = do
  pid <- ensurePartyRecord now name mlegal
  _ <- upsert (PartyRole pid role True) [PartyRoleActive =. True]
  upsertToken token pid (Just (roleLabel role))
  ensureCredential pid uname pwd
  pure pid

ensurePartyRecord :: UTCTime -> Text -> Maybe Text -> SqlPersistT IO (Key Party)
ensurePartyRecord now name mlegal = do
  existing <- selectFirst [PartyDisplayName ==. name] []
  case existing of
    Just (Entity pid _) -> pure pid
    Nothing -> insert $ Party mlegal name False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now

upsertToken :: Text -> PartyId -> Maybe Text -> SqlPersistT IO ()
upsertToken token pid label = do
  mTok <- getBy (UniqueApiToken token)
  case mTok of
    Just (Entity tokId _) -> update tokId [ ApiTokenPartyId =. pid, ApiTokenActive =. True, ApiTokenLabel =. label ]
    Nothing -> do
      _ <- insert $ ApiToken token pid label True
      pure ()

ensureCredential :: PartyId -> Text -> Text -> SqlPersistT IO ()
ensureCredential pid uname pwd = do
  hashed <- liftIO (hashPasswordText pwd)
  _ <- upsert (UserCredential pid uname hashed True)
         [ UserCredentialPasswordHash =. hashed
         , UserCredentialActive =. True
         ]
  pure ()

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

roleLabel :: RoleEnum -> Text
roleLabel = T.pack . show

ensureDropdownOption
  :: UTCTime
  -> (Text, Text, Maybe Text, Maybe Int)
  -> SqlPersistT IO ()
ensureDropdownOption now (categoryKey, valueTxt, mLabel, sortOrder) = do
  let labelValue = fromMaybe valueTxt mLabel
  existing <- selectFirst
    [ ME.DropdownOptionCategory ==. categoryKey
    , ME.DropdownOptionValue ==. valueTxt
    ]
    []
  case existing of
    Just (Entity optionId _) ->
      update optionId
        [ ME.DropdownOptionLabel =. Just labelValue
        , ME.DropdownOptionSortOrder =. sortOrder
        , ME.DropdownOptionActive =. True
        , ME.DropdownOptionUpdatedAt =. now
        ]
    Nothing -> do
      _ <- insert DropdownOption
        { dropdownOptionCategory  = categoryKey
        , dropdownOptionValue     = valueTxt
        , dropdownOptionLabel     = Just labelValue
        , dropdownOptionActive    = True
        , dropdownOptionSortOrder = sortOrder
        , dropdownOptionCreatedAt = now
        , dropdownOptionUpdatedAt = now
        }
      pure ()

ensureSubjectRecord :: Text -> Bool -> SqlPersistT IO (Key Trials.Subject)
ensureSubjectRecord name isActive = do
  existing <- getBy (Trials.UniqueSubjectName name)
  case existing of
    Just (Entity sid subj) -> do
      when (Trials.subjectActive subj /= isActive) $
        update sid [Trials.SubjectActive =. isActive]
      pure sid
    Nothing -> insert Trials.Subject
      { Trials.subjectName   = name
      , Trials.subjectActive = isActive
      }

ensureSubjectRoomPref :: Key Trials.Subject -> ResourceId -> Int -> SqlPersistT IO ()
ensureSubjectRoomPref subjectId roomId priority = do
  void $
    upsert (Trials.SubjectRoomPreference subjectId roomId priority)
      [ Trials.SubjectRoomPreferencePriority =. priority ]

ensureTeacherSubjectLink :: PartyId -> Key Trials.Subject -> SqlPersistT IO ()
ensureTeacherSubjectLink teacherId subjectId = do
  _ <- insertUnique $ Trials.TeacherSubject
    { Trials.teacherSubjectTeacherId = teacherId
    , Trials.teacherSubjectSubjectId = subjectId
    , Trials.teacherSubjectLevelMin  = Nothing
    , Trials.teacherSubjectLevelMax  = Nothing
    }
  pure ()

ensureTeacherAvailabilitySlot
  :: UTCTime
  -> PartyId
  -> Key Trials.Subject
  -> ResourceId
  -> UTCTime
  -> UTCTime
  -> SqlPersistT IO ()
ensureTeacherAvailabilitySlot createdAt teacherId subjectId roomId startAt endAt = do
  existing <- selectFirst
    [ Trials.TeacherAvailabilityTeacherId ==. teacherId
    , Trials.TeacherAvailabilitySubjectId ==. subjectId
    , Trials.TeacherAvailabilityStartAt   ==. startAt
    , Trials.TeacherAvailabilityEndAt     ==. endAt
    ]
    []
  case existing of
    Just _  -> pure ()
    Nothing -> do
      void $ insert Trials.TeacherAvailability
        { Trials.teacherAvailabilityTeacherId = teacherId
        , Trials.teacherAvailabilitySubjectId = subjectId
        , Trials.teacherAvailabilityRoomId    = roomId
        , Trials.teacherAvailabilityStartAt   = startAt
        , Trials.teacherAvailabilityEndAt     = endAt
        , Trials.teacherAvailabilityNotes     = Nothing
        , Trials.teacherAvailabilityCreatedAt = createdAt
        }
