module Api (API, api, server) where

import Servant
import Routes.Invoices (InvoiceAPI, DemoAPI, invoiceServer, demoServer)
import Routes.Health (HealthAPI, healthServer)

-- Full API
 type API = HealthAPI
        :<|> InvoiceAPI
        :<|> DemoAPI
        :<|> "invoices" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = healthServer :<|> invoiceServer :<|> demoServer :<|> serveDirectoryFileServer "/app/public/invoices"
