{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Api (api, server)
import Network.Wai.Middleware.Cors
import Network.Wai (Application)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG
import System.Environment (getEnv)
import Data.String (fromString)

mkConn :: IO Connection
mkConn = do
  url <- getEnv "DATABASE_URL"
  PG.connectPostgreSQL (fromString url)

app :: Connection -> Application
app conn = serve api (server conn)

main :: IO ()
main = do
  conn <- mkConn
  let policy = simpleCorsResourcePolicy
        { corsOrigins = Just (["https://tdfui.pages.dev"], True)
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }
  run 8080 $ cors (const $ Just policy) (app conn)
