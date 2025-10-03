{-# LANGUAGE OverloadedStrings #-}
module TDF.Seed where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import           TDF.Models

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
           _ <- insertUnique $ Party mlegal disp False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now
           pure ()
        ) artists

  let teachers =
        [ ("César Galarza", Nothing)
        , ("Fabricio Alomía", Nothing)
        , ("Juan Ledesma", Nothing)
        ]
  mapM_ (\(disp, mlegal) -> do
           pid <- insert $ Party mlegal disp False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now
           _ <- insertUnique (PartyRole pid Teacher True)
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

  -- Assets (gear)
  let assets = ["Korg SV1","Prophet 08","Moog Subsequent 37","Gibson Ripper Bass","Model 1","SVT","Tone Hammer"]
  mapM_ (\a -> insertUnique (Asset Nothing a "Instrument" Nothing Nothing Nothing Nothing (Just "Studio") Nothing False Nothing True) >> pure ()) assets

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
