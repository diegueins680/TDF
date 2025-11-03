{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Routes.Invoices where

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


type InvoiceAPI = 
  "invoices" :> Capture "sessionId" String
             :> "generate"
             :> ReqBody '[JSON] InvoiceReq
             :> Post '[JSON] FilePath


invoiceServer :: Server InvoiceAPI
invoiceServer sid req = do
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
