{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad            (forM_, unless, when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Network.Wai.Handler.Warp as Warp
import           Data.ByteString.Char8    (pack)
import qualified Data.Text                as T
import           Data.Time                (UTCTime, getCurrentTime)
import           Database.Persist         (Entity(..), get, insert, selectFirst, (==.))
import           Database.Persist.Sql     (PersistValue(..), Single(..), SqlPersistT, fromSqlKey, getMigration, rawExecute, rawSql,
                                           runSqlPool, toSqlKey, unSingle, Migration)
import           Text.Read                (readMaybe)

-- NEW: CORS middleware
import           Network.Wai.Middleware.Cors
                 ( cors
                 , simpleCorsResourcePolicy
                 , CorsResourcePolicy(..)
                 , simpleHeaders
                 )

import           TDF.Config     (appPort, dbConnString, loadConfig)
import           TDF.DB         (Env(..), makePool)
import           TDF.Models
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Server     (mkApp)

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- makePool (pack (dbConnString cfg))
  putStrLn "Running DB migrations..."
  runSqlPool migrationSteps pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  -- Permissive CORS for development (tighten in production)
  -- - Explicitly allow local frontend origins so credentials work when needed.
  -- - Keep default simple headers and add Authorization for authenticated calls.
  let allowedOrigins =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "http://localhost:4173"
        , "http://127.0.0.1:4173"
        , "http://localhost:3000"
        , "http://127.0.0.1:3000"
        ]
      corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = "Authorization" : simpleHeaders
          , corsMethods        = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
          , corsOrigins        = Just (allowedOrigins, True)
          -- To restrict in prod, overwrite 'allowedOrigins' with trusted hosts.
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)
  where
    migrationSteps = do
      runSafeMigration migrateAll
      rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
      upgradeBandsToParties
      runSafeMigration migrateExtra

    upgradeBandsToParties :: SqlPersistT IO ()
    upgradeBandsToParties = do
      ensureColumn "band" "party_id" "BIGINT"
      ensureColumn "band_member" "party_id" "BIGINT"
      convertBands
      convertBandMembers
      ensureLegacyTables
      rawExecute "ALTER TABLE band ALTER COLUMN party_id SET NOT NULL" []
      rawExecute "ALTER TABLE band_member ALTER COLUMN party_id SET NOT NULL" []
      rawExecute "ALTER TABLE band_member DROP CONSTRAINT IF EXISTS unique_band_member" []
      ensureConstraint "band" "unique_band_party"
        "ALTER TABLE band ADD CONSTRAINT unique_band_party UNIQUE (party_id)"
      ensureConstraint "band_member" "unique_band_member"
        "ALTER TABLE band_member ADD CONSTRAINT unique_band_member UNIQUE (band_id, party_id)"
      ensureConstraint "band" "band_party_id_fkey"
        "ALTER TABLE band ADD CONSTRAINT band_party_id_fkey FOREIGN KEY (party_id) REFERENCES party(id) ON DELETE CASCADE"
      ensureConstraint "band_member" "band_member_party_id_fkey"
        "ALTER TABLE band_member ADD CONSTRAINT band_member_party_id_fkey FOREIGN KEY (party_id) REFERENCES party(id) ON DELETE CASCADE"
      hasPartyRef <- columnExists "band_member" "party_ref"
      when hasPartyRef $
        rawExecute "ALTER TABLE band_member DROP COLUMN party_ref" []

    ensureColumn :: T.Text -> T.Text -> T.Text -> SqlPersistT IO ()
    ensureColumn table column columnType = do
      exists <- columnExists table column
      unless exists $
        rawExecute (
          "ALTER TABLE " <> table <> " ADD COLUMN " <> column <> " " <> columnType
        ) []

    columnExists :: T.Text -> T.Text -> SqlPersistT IO Bool
    columnExists table column = do
      let sql = "SELECT 1::INT FROM information_schema.columns WHERE table_name = ? AND column_name = ? LIMIT 1"
      res <- rawSql sql [PersistText table, PersistText column] :: SqlPersistT IO [Single Int]
      pure (not (null res))

    ensureConstraint :: T.Text -> T.Text -> T.Text -> SqlPersistT IO ()
    ensureConstraint table constraint statement = do
      exists <- constraintExists table constraint
      unless exists $
        rawExecute statement []

    constraintExists :: T.Text -> T.Text -> SqlPersistT IO Bool
    constraintExists table constraint = do
      let sql = "SELECT 1::INT\n                 FROM pg_constraint c\n                 JOIN pg_class t ON c.conrelid = t.oid\n                 JOIN pg_namespace n ON t.relnamespace = n.oid\n                 WHERE t.relname = ? AND c.conname = ? LIMIT 1"
      res <- rawSql sql [PersistText table, PersistText constraint] :: SqlPersistT IO [Single Int]
      pure (not (null res))

    convertBands :: SqlPersistT IO ()
    convertBands = do
      rows <- rawSql
        "SELECT id::text, name, label_artist, notes FROM band WHERE party_id IS NULL"
        [] :: SqlPersistT IO [( Single T.Text
                               , Single T.Text
                               , Single Bool
                               , Single (Maybe T.Text)
                               )]
      unless (null rows) $ do
        now <- liftIO getCurrentTime
        forM_ rows $ \(bandIdTxt, nameTxt, labelArtistTxt, notesTxt) -> do
          let bandId        = unSingle bandIdTxt
              name          = unSingle nameTxt
              isLabelArtist = unSingle labelArtistTxt
              legacyNotes   = unSingle notesTxt
          partyKey <- insert Party
            { partyLegalName        = Nothing
            , partyDisplayName      = name
            , partyIsOrg            = True
            , partyTaxId            = Nothing
            , partyPrimaryEmail     = Nothing
            , partyPrimaryPhone     = Nothing
            , partyWhatsapp         = Nothing
            , partyInstagram        = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes            = buildNote isLabelArtist legacyNotes
            , partyCreatedAt        = now
            }
          rawExecute
            "UPDATE band SET party_id = ? WHERE id = ?::uuid"
            [PersistInt64 (fromSqlKey partyKey), PersistText bandId]

    convertBandMembers :: SqlPersistT IO ()
    convertBandMembers = do
      hasPartyRef <- columnExists "band_member" "party_ref"
      when hasPartyRef $ do
        rows <- rawSql
          "SELECT id::text, party_ref FROM band_member WHERE party_id IS NULL"
          [] :: SqlPersistT IO [(Single T.Text, Single (Maybe T.Text))]
        now <- liftIO getCurrentTime
        forM_ rows $ \(memberIdTxt, refTxt) -> do
          let memberId = unSingle memberIdTxt
              mRef     = fmap T.strip (unSingle refTxt)
          mKey <- resolvePartyRef now mRef
          case mKey of
            Nothing -> pure ()
            Just partyKey ->
              rawExecute
                "UPDATE band_member SET party_id = ? WHERE id = ?::uuid"
                [PersistInt64 (fromSqlKey partyKey), PersistText memberId]

    resolvePartyRef :: UTCTime -> Maybe T.Text -> SqlPersistT IO (Maybe (Key Party))
    resolvePartyRef _ Nothing = pure Nothing
    resolvePartyRef now (Just refTxt)
      | T.null refTxt = createAnonymousParty now "Band member"
      | otherwise = case readMaybe (T.unpack refTxt) of
          Just intId -> do
            let partyKey = toSqlKey intId :: Key Party
            existing <- get partyKey
            case existing of
              Just _  -> pure (Just partyKey)
              Nothing -> createAnonymousParty now ("Migrated member " <> T.pack (show intId))
          Nothing -> do
            existing <- selectFirst [PartyDisplayName ==. refTxt] []
            case existing of
              Just (Entity key _) -> pure (Just key)
              Nothing             -> createAnonymousParty now refTxt

    createAnonymousParty :: UTCTime -> T.Text -> SqlPersistT IO (Maybe (Key Party))
    createAnonymousParty now label = do
      let name = if T.null (T.strip label) then "Migrated Band Member" else label
      mExisting <- selectFirst [PartyDisplayName ==. name] []
      case mExisting of
        Just (Entity key _) -> pure (Just key)
        Nothing -> do
          key <- insert Party
            { partyLegalName        = Nothing
            , partyDisplayName      = name
            , partyIsOrg            = False
            , partyTaxId            = Nothing
            , partyPrimaryEmail     = Nothing
            , partyPrimaryPhone     = Nothing
            , partyWhatsapp         = Nothing
            , partyInstagram        = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes            = Just "Migrated auto-generated party"
            , partyCreatedAt        = now
            }
          pure (Just key)

    buildNote :: Bool -> Maybe T.Text -> Maybe T.Text
    buildNote isLabelArtist legacyNotes =
      let base = if isLabelArtist then "Label artist" else "Independent"
      in case nonEmpty legacyNotes of
           Nothing   -> Just base
           Just note -> Just (base <> "; " <> note)

    nonEmpty :: Maybe T.Text -> Maybe T.Text
    nonEmpty = maybe Nothing $ \val -> let trimmed = T.strip val in if T.null trimmed then Nothing else Just trimmed

    runSafeMigration :: Migration -> SqlPersistT IO ()
    runSafeMigration migration = do
      statements <- getMigration migration
      let allowed = filter (not . isUnsafe) statements
          skipped = length statements - length allowed
      forM_ allowed $ \stmt -> rawExecute stmt []
      unless (skipped == 0) $
        liftIO $ putStrLn "Skipped legacy-incompatible migration statements"

    isUnsafe :: T.Text -> Bool
    isUnsafe stmt =
      any (`T.isInfixOf` stmt)
        [ "DROP COLUMN"
        , "\"asset\""
        , "\"asset_checkout\""
        , "\"maintenance_ticket\""
        , "\"stock_item\""
        , "\"room_default_gear\""
        , "\"asset_kit_member\""
        , "\"asset_audit\""
        , "\"maintenance_attachment\""
        , "\"stock_movement\""
        , "\"session\""
        , "\"session_room\""
        , "\"session_deliverable\""
        , "\"input_list\""
        , "\"input_list_version\""
        , "\"input_list_template\""
        , "\"input_list_template_row\""
        , "\"input_row\""
        ]

    ensureLegacyTables :: SqlPersistT IO ()
    ensureLegacyTables = do
      rawExecute
        "CREATE TABLE IF NOT EXISTS asset\n         ( id BIGSERIAL PRIMARY KEY\n         , sku TEXT NULL\n         , name TEXT NOT NULL\n         , category TEXT NOT NULL\n         , serial_number TEXT NULL\n         , purchase_date DATE NULL\n         , purchase_vendor TEXT NULL\n         , purchase_cost_cents INT NULL\n         , location TEXT NULL\n         , condition TEXT NULL\n         , insured BOOL NOT NULL DEFAULT false\n         , insurance_policy TEXT NULL\n         , active BOOL NOT NULL DEFAULT true\n         );"
        []
      rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS unique_serial ON asset(serial_number)" []
      rawExecute
        "CREATE TABLE IF NOT EXISTS asset_checkout\n         ( id BIGSERIAL PRIMARY KEY\n         , asset_id BIGINT NOT NULL\n         , booking_id BIGINT NULL\n         , party_id BIGINT NULL\n         , out_at TIMESTAMPTZ NOT NULL DEFAULT now()\n         , due_at TIMESTAMPTZ NULL\n         , in_at TIMESTAMPTZ NULL\n         , notes TEXT NULL\n         );"
        []
      rawExecute
        "CREATE TABLE IF NOT EXISTS maintenance_ticket\n         ( id BIGSERIAL PRIMARY KEY\n         , asset_id BIGINT NOT NULL\n         , opened_at TIMESTAMPTZ NOT NULL DEFAULT now()\n         , status TEXT NOT NULL\n         , description TEXT NOT NULL\n         , cost_cents INT NULL\n         , next_service_at DATE NULL\n         );"
        []
      rawExecute
        "CREATE TABLE IF NOT EXISTS stock_item\n         ( id BIGSERIAL PRIMARY KEY\n         , sku TEXT NOT NULL\n         , name TEXT NOT NULL\n         , unit TEXT NOT NULL DEFAULT 'Pcs'\n         , min_level INT NULL\n         , reorder_point INT NULL\n         , vendor TEXT NULL\n         , active BOOL NOT NULL DEFAULT true\n         );"
        []
