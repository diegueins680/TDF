{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Routes.Invoices (InvoiceAPI, DemoAPI, invoiceServer, demoServer) where

import Servant
import Servant.Server (throwError, err500, errBody)
import Data.Aeson
import GHC.Generics
import System.Process (callCommand)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.IO.Class (liftIO)

data InvoiceReq = InvoiceReq
  { sessionId :: String
  , client :: Value
  , items :: Value
  , summary :: Value
  , payment :: Value
  } deriving (Generic, Show)

instance FromJSON InvoiceReq
instance ToJSON InvoiceReq

-- Public APIs
type InvoiceAPI = 
  "invoices" :> Capture "sessionId" String
             :> "generate"
             :> ReqBody '[JSON] InvoiceReq
             :> Post '[JSON] FilePath

-- Demo endpoint without body
type DemoAPI = 
  "invoices" :> "demo" :> "generate" :> Post '[JSON] FilePath


invoiceServer :: Server InvoiceAPI
invoiceServer sid req = generateInvoice sid req


demoServer :: Server DemoAPI
demoServer = generateInvoice "demo" defaultReq
  where
    defaultReq = InvoiceReq
      { sessionId = "demo"
      , client = object [ "name" .= String "Cliente Demo", "taxId" .= String "0000000000" ]
      , items = toJSON [ object [ "desc" .= String "Hora de estudio", "qty" .= Number 2, "unit_price" .= Number 25, "total" .= Number 50 ] ]
      , summary = object [ "subtotal" .= Number 50, "tax_rate" .= String "12%", "tax" .= Number 6, "total" .= Number 56 ]
      , payment = object [ "terms" .= String "50% anticipo / 50% previo a entrega", "instructions" .= String "Transferencia a TDF Records S.A.S." ]
      }


-- Impl
generateInvoice :: String -> InvoiceReq -> Handler FilePath
generateInvoice sid req = do
  let texFile = "/app/templates/invoice.tex"
  let logoFile = "/app/templates/tdf-logo.pdf"
  let outDir  = "/app/public/invoices"
  let jsonFile = "/tmp/invoice.json"
  let pdfTmp = "/tmp/invoice_filled.pdf"
  let texTmp = "/tmp/invoice_filled.tex"
  let pdfOut = outDir ++ "/" ++ sid ++ ".pdf"

  okLogo <- liftIO $ doesFileExist logoFile
  if not okLogo
    then throwError $ err500 { errBody = "Missing logo file: /app/templates/tdf-logo.pdf" }
    else pure ()

  liftIO $ createDirectoryIfMissing True outDir
  liftIO $ BL.writeFile jsonFile (encode req)
  liftIO $ callCommand $ unwords [ "mustache", jsonFile, texFile, ">", texTmp ]
  liftIO $ callCommand "latexmk -pdf -quiet -output-directory=/tmp /tmp/invoice_filled.tex"
  liftIO $ copyFile pdfTmp pdfOut
  pure pdfOut
