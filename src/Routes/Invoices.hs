{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Routes.Invoices where

import Servant
import Data.Aeson
import GHC.Generics
import System.Process (callCommand)
import System.Directory (copyFile)
import qualified Data.ByteString.Lazy.Char8 as BL

data InvoiceReq = InvoiceReq
  { sessionId :: String
  , client :: Value
  , items :: Value
  , summary :: Value
  , payment :: Value
  } deriving (Generic, Show)

instance FromJSON InvoiceReq
instance ToJSON InvoiceReq


type InvoiceAPI = 
  "invoices" :> Capture "sessionId" String
             :> "generate"
             :> ReqBody '[JSON] InvoiceReq
             :> Post '[JSON] FilePath


invoiceServer :: Server InvoiceAPI
invoiceServer sid req = do
  let texFile = "/app/templates/invoice.tex"
  let jsonFile = "/tmp/invoice.json"
  let pdfOut = "/app/public/invoices/" ++ sid ++ ".pdf"
  BL.writeFile jsonFile (encode req)
  callCommand $ unwords [ "mustache", jsonFile, texFile, ">", "/tmp/invoice_filled.tex" ]
  callCommand "latexmk -pdf -quiet -output-directory=/tmp /tmp/invoice_filled.tex"
  copyFile "/tmp/invoice_filled.pdf" pdfOut
  pure pdfOut
