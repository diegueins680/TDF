{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.Meta
  ( MetaAPI
  , metaServer
  , BuildInfo(..)
  , redocIndex
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON)
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Version               (showVersion)
import           GHC.Generics               (Generic)
import           Servant
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Paths_tdf_hq               as Paths

-- | Basic build info for the About dialog.
data BuildInfo = BuildInfo
  { app     :: T.Text
  , version :: T.Text
  , builtAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON BuildInfo

type MetaAPI =
       "version"      :> Get '[JSON] BuildInfo
  :<|> "openapi.yaml" :> Get '[PlainText] T.Text
  :<|> "docs"         :> Get '[PlainText] T.Text

metaServer :: Server MetaAPI
metaServer = versionH :<|> openapiH :<|> docsH
  where
    versionH = do
      now <- liftIO getCurrentTime
      pure BuildInfo
        { app     = "tdf-hq"
        , version = T.pack (showVersion Paths.version)
        , builtAt = now
        }
    openapiH = liftIO $ TIO.readFile "docs/openapi/lessons-and-receipts.yaml"
    docsH    = pure (T.pack redocIndex)

-- | Minimal Redoc index that points to /openapi.yaml on the same server.
redocIndex :: String
redocIndex = unlines
  [ "<!doctype html>"
  , "<html>"
  , "<head>"
  , "  <meta charset='utf-8'/>"
  , "  <meta name='viewport' content='width=device-width, initial-scale=1'/>"
  , "  <title>TDF API Docs</title>"
  , "  <script src='https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js'></script>"
  , "</head>"
  , "<body>"
  , "  <redoc spec-url='/openapi.yaml'></redoc>"
  , "</body>"
  , "</html>"
  ]

{-
To mount these routes in your Servant 'API' and 'Server', add something like:

  import TDF.Meta (MetaAPI, metaServer)
  type API = MetaAPI :<|> ExistingAPI
  server :: Server API
  server = metaServer :<|> existingServer

If your app exposes 'app :: Application' instead, wrap your existing server:

  import Network.Wai (Application)
  import Servant
  app :: Application
  app = serve (Proxy :: Proxy MetaAPI) metaServer <|> existingApp

Adjust the wiring to your codebase as needed.
-}
