{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Api (API, api, server)

main :: IO ()
main = run 8080 (serve api server)
