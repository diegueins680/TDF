{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Data.ByteString.Char8    (pack)
import           Database.Persist.Sql     (SqlPersistT, rawExecute, runMigration, runSqlPool)

import           Network.Wai.Middleware.Cors
                 ( cors
                 , simpleCorsResourcePolicy
                 , CorsResourcePolicy(..)
                 , simpleHeaders
                 )

import           TDF.Config     (appPort, dbConnString, loadConfig)
import           TDF.DB         (Env(..), makePool)
import           TDF.Models     (migrateAll)
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Server     (mkApp)

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- makePool (pack (dbConnString cfg))
  putStrLn "Running DB migrations..."
  runSqlPool initializeSchema pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  -- Permissive CORS for development (tighten in production)
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
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)

initializeSchema :: SqlPersistT IO ()
initializeSchema = do
  rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
  runMigration migrateAll
  runMigration migrateExtra
