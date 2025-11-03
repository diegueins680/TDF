{-# LANGUAGE OverloadedStrings #-}
module Routes.InvoicesPG (InvoicePGAPI, serverInvoicesPG) where

import Servant
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

-- Minimal Postgres-backed API surface (implementation in next commit)
-- POST   /invoices/:sessionId/generate
-- GET    /invoices/by-session/:sessionId
-- GET    /invoices/:id

 type InvoicePGAPI =
        "invoices" :> Capture "sessionId" T.Text :> "generate" :> ReqBody '[JSON] Value :> Post '[JSON] Value
   :<|> "invoices" :> "by-session" :> Capture "sessionId" T.Text :> Get '[JSON] Value
   :<|> "invoices" :> Capture "id" T.Text :> Get '[JSON] Value

 serverInvoicesPG :: Connection -> Server InvoicePGAPI
 serverInvoicesPG _conn = postGen :<|> getBySession :<|> getById
  where
    postGen sid _body = pure (object ["ok" .= True, "sessionId" .= sid])
    getBySession sid  = pure (object ["ok" .= True, "sessionId" .= sid])
    getById iid       = pure (object ["ok" .= True, "id" .= iid])
