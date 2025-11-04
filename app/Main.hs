{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad            (when)
import           Data.ByteString.Char8    (pack)
import           Data.Char                (isSpace)
import           Data.List                (dropWhileEnd)
import           Database.Persist.Sql     (SqlPersistT, rawExecute, runMigration, runSqlPool)
import           System.Environment       (lookupEnv)

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

  let allowedOriginsBase =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "http://localhost:4173"
        , "http://127.0.0.1:4173"
        , "http://localhost:3000"
        , "http://127.0.0.1:3000"
        , "http://localhost:5174"
        , "http://127.0.0.1:5174"
        , "https://tdf-ui.onrender.com"
        , "https://tdf-7t2qa.onrender.com"
        , "https://tdfui.pages.dev"
        ]
  envOrigins <- lookupEnv "ALLOW_ORIGINS"
  envOrigin  <- lookupEnv "ALLOW_ORIGIN"
  let fromListEnv =
        maybe [] (map pack . splitComma) envOrigins
      fromOneEnv =
        maybe [] (\origin -> [pack origin]) envOrigin
      allowedOrigins = allowedOriginsBase <> fromListEnv <> fromOneEnv
      allowedHeaders =
        "Authorization"
        : "Content-Type"
        : "X-Requested-With"
        : simpleHeaders
      corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = allowedHeaders
          , corsMethods        = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
          , corsOrigins        = Just (allowedOrigins, True)
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)

-- | Split a comma-separated list into trimmed entries.
splitComma :: String -> [String]
splitComma = go . dropWhile isSpace
  where
    go [] = []
    go s =
      let (h, t) = break (== ',') s
          h'     = trim h
      in if null t
           then [h']
           else h' : go (drop 1 t)
    trim = dropWhileEnd isSpace . dropWhile isSpace

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
