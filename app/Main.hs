{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Data.ByteString.Char8 (pack)
import           Database.Persist.Sql (runSqlPool, runMigration)

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
  Warp.run (appPort cfg) (mkApp Env{ envPool = pool, envConfig = cfg })
