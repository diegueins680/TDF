
{-# LANGUAGE OverloadedStrings #-}
module TDF.DB where

import           Control.Monad.Logger (runStdoutLoggingT)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (SqlPersistT, ConnectionPool)
import           Data.ByteString (ByteString)

import           TDF.Config

data Env = Env
  { envPool   :: ConnectionPool
  , envConfig :: AppConfig
  }

makePool :: ByteString -> IO ConnectionPool
makePool conn = runStdoutLoggingT $ createPostgresqlPool conn 10

runMigrations :: (SqlPersistT IO () ) -> SqlPersistT IO ()
runMigrations = id
