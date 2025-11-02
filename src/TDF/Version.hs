{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Version
  ( VersionInfo(..)
  , getVersionInfo
  ) where

import           Data.Aeson         (ToJSON(..), object, (.=))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Version       (showVersion)
import           GHC.Generics       (Generic)
import           Paths_tdf_hq       (version)
import           System.Environment (lookupEnv)

data VersionInfo = VersionInfo
  { name      :: Text
  , appVer    :: Text
  , commit    :: Text
  , buildTime :: Text
  } deriving (Show, Generic)

instance ToJSON VersionInfo where
  toJSON v = object
    [ "name"      .= name v
    , "version"   .= appVer v
    , "commit"    .= commit v
    , "buildTime" .= buildTime v
    ]

getVersionInfo :: IO VersionInfo
getVersionInfo = do
  sha   <- maybe "dev" T.pack <$> lookupEnv "GIT_SHA"
  btime <- maybe ""   T.pack <$> lookupEnv "BUILD_TIME"
  pure VersionInfo
    { name      = "tdf-hq"
    , appVer    = T.pack (showVersion version)
    , commit    = sha
    , buildTime = btime
    }
