{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           System.Environment (lookupEnv)
import           Data.Maybe (fromMaybe)

data AppConfig = AppConfig
  { dbHost  :: String
  , dbPort  :: String
  , dbUser  :: String
  , dbPass  :: String
  , dbName  :: String
  , appPort :: Int
  } deriving (Show)

dbConnString :: AppConfig -> String
dbConnString cfg =
  "host="    <> dbHost cfg    <>
  " port="   <> dbPort cfg    <>
  " user="   <> dbUser cfg    <>
  " password=" <> dbPass cfg  <>
  " dbname=" <> dbName cfg    -- no 'pool=' here; pooling is managed by createPostgresqlPool

loadConfig :: IO AppConfig
loadConfig = do
  h  <- get "DB_HOST" "127.0.0.1"
  p  <- get "DB_PORT" "5432"
  u  <- get "DB_USER" "postgres"
  w  <- get "DB_PASS" "postgres"
  d  <- get "DB_NAME" "tdf_hq"
  ap <- get "APP_PORT" "8080"
  pure AppConfig
    { dbHost = h, dbPort = p, dbUser = u, dbPass = w, dbName = d, appPort = read ap }
  where
    get k def = fmap (fromMaybe def) (lookupEnv k)
