{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad            (when)
import           Data.ByteString.Char8    (pack)
import           Database.Persist.Sql     (SqlPersistT, rawExecute, runMigration, runSqlPool)

import           Network.Wai.Middleware.Cors
                 ( cors
                 , simpleCorsResourcePolicy
                 , CorsResourcePolicy(..)
                 , simpleHeaders
                 )

import           TDF.Config     (appPort, dbConnString, loadConfig, resetDb, seedDatabase)
import           TDF.DB         (Env(..), makePool)
import           TDF.Models     (migrateAll)
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Trials.Models (migrateTrials)
import           TDF.Server     (mkApp)
import           TDF.Seed       (seedAll)

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- makePool (pack (dbConnString cfg))
  if resetDb cfg
    then do
      putStrLn "Resetting DB schema..."
      runSqlPool resetSchema pool
    else
      putStrLn "RESET_DB disabled, preserving existing schema."
  putStrLn "Running DB migrations..."
  runSqlPool runMigrations pool
  when (seedDatabase cfg) $ do
    putStrLn "Seeding initial data..."
    runSqlPool seedAll pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  let allowedOrigins =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "http://localhost:4173"
        , "http://127.0.0.1:4173"
        , "http://localhost:3000"
        , "http://127.0.0.1:3000"
        , "https://tdf-ui.onrender.com"
        , "https://tdf-7t2qa.onrender.com"
        ]
      corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = "Authorization" : simpleHeaders
          , corsMethods        = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
          , corsOrigins        = Just (allowedOrigins, True)
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)

resetSchema :: SqlPersistT IO ()
resetSchema = do
  rawExecute "DROP EXTENSION IF EXISTS pgcrypto" []
  rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
  rawExecute "GRANT ALL ON SCHEMA public TO CURRENT_USER" []
  rawExecute "GRANT ALL ON SCHEMA public TO public" []

runMigrations :: SqlPersistT IO ()
runMigrations = do
  rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
  runMigration migrateAll
  runMigration migrateExtra
  runMigration migrateTrials
