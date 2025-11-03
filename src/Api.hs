module Api (API, api, server) where

import Servant
import Routes.Invoices (InvoiceAPI, invoiceServer)

-- | Full public API
--  POST /invoices/:sessionId/generate
--  GET  /invoices/*  (serves generated PDFs)
 type API = InvoiceAPI
         :<|> "invoices" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = invoiceServer :<|> serveDirectoryFileServer "/app/public/invoices"
