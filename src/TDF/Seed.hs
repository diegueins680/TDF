{-# LANGUAGE OverloadedStrings #-}
module TDF.Seed where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import           TDF.Models
import           TDF.ModelsExtra (DropdownOption(..))
import qualified TDF.ModelsExtra as ME

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
  mapM_ (\(disp, mlegal) -> do
           pid <- ensurePartyRecord now disp mlegal
           _ <- upsert (PartyRole pid Teacher True)
             [ PartyRoleActive =. True ]
           pure ()
        ) teachers

  -- Service Catalog
  let svc name kind pm rate tax = ServiceCatalog name kind pm rate tax True
  _ <- insertUnique (svc "Recording" Recording Hourly Nothing (Just 1200))
  _ <- insertUnique (svc "Mixing" Mixing PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Mastering" Mastering PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Rehearsal" Rehearsal Hourly (Just (15*100)) (Just 1200))
  _ <- insertUnique (svc "Classes" Classes Package Nothing (Just 1200))
  _ <- insertUnique (svc "Event Production" EventProduction Quote Nothing (Just 1200))

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
  mapM_ (\r -> insertUnique (Resource r (slugify r) Room Nothing True) >> pure ()) rooms

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
