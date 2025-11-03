{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Api (api, server)
import Network.Wai.Middleware.Cors
import Network.Wai (Application)

app :: Application
app = serve api server

main :: IO ()
main = do
  let policy = simpleCorsResourcePolicy
        { corsOrigins = Just (["https://tdfui.pages.dev"], True)
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }
  run 8080 $ cors (const $ Just policy) app
