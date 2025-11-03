module Api (API, api, server) where

import Servant
import Routes.Invoices (InvoiceAPI, DemoAPI, invoiceServer, demoServer)
import Routes.InvoicesPG (InvoicePGAPI, serverInvoicesPG)
import Routes.Health (HealthAPI, healthServer)
import Database.PostgreSQL.Simple (Connection)

 type API = HealthAPI
        :<|> InvoiceAPI
        :<|> DemoAPI
        :<|> InvoicePGAPI
        :<|> "invoices" :> Raw

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server conn = healthServer :<|> invoiceServer :<|> demoServer :<|> serverInvoicesPG conn :<|> serveDirectoryFileServer "/app/public/invoices"
