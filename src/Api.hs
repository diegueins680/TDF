module Api (API, api, server) where

import Servant
import Routes.Invoices (InvoiceAPI, invoiceServer)

-- | Compose your full API here
 type API = InvoiceAPI

api :: Proxy API
api = Proxy

server :: Server API
server = invoiceServer
