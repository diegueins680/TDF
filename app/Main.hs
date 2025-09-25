{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Data.ByteString.Char8 (pack)
import           Database.Persist.Sql (runSqlPool, runMigration)

-- NEW: CORS middleware
import           Network.Wai.Middleware.Cors
                 ( cors
                 , simpleCorsResourcePolicy
                 , CorsResourcePolicy(..)
                 )

import           TDF.Config (loadConfig, dbConnString, appPort)
import           TDF.DB     (makePool, Env(..))
import           TDF.Models (migrateAll)
import           TDF.Server (mkApp)

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- makePool (pack (dbConnString cfg))
  putStrLn "Running DB migrations..."
  runSqlPool (runMigration migrateAll) pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  -- Permissive CORS for development (tighten in production)
  -- - Allows any Origin (leave 'corsOrigins = Nothing')
  -- - Allows common methods & headers
  let corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type", "Authorization"]
          , corsMethods        = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
          -- To restrict in prod, set a whitelist like:
          -- , corsOrigins = Just (["https://your-frontend.example"], True)
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)
