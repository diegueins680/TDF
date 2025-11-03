{-# LANGUAGE OverloadedStrings #-}
module Routes.Health (HealthAPI, healthServer) where

import Servant

 type HealthAPI = "health" :> Get '[PlainText] String

 healthServer :: Server HealthAPI
 healthServer = pure "ok"
