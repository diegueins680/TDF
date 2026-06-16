{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.SRI
  ( SriScriptCustomer(..)
  , SriScriptLine(..)
  , SriScriptRequest(..)
  , SriScriptResult(..)
  , runSriInvoiceScript
  ) where

import           Data.Aeson (ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data SriScriptCustomer = SriScriptCustomer
  { ruc       :: Text
  , legalName :: Text
  , email     :: Maybe Text
  , phone     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriScriptCustomer

data SriScriptLine = SriScriptLine
  { code              :: Maybe Text
  , auxiliaryCode     :: Maybe Text
  , description       :: Text
  , quantity          :: Int
  , unitCents         :: Int
  , taxBps            :: Maybe Int
  , sriAdditionalInfo :: Maybe Text
  , sriIvaCode        :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriScriptLine

data SriScriptRequest = SriScriptRequest
  { customer            :: SriScriptCustomer
  , lines               :: [SriScriptLine]
  , establishment       :: Text
  , emissionPoint       :: Text
  , paymentMode         :: Text
  , signAndSend         :: Bool
  , certificatePassword :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriScriptRequest

data SriScriptResult = SriScriptResult
  { sirStatus              :: Text
  , sirAuthorizationNumber :: Maybe Text
  , sirInvoiceNumber       :: Maybe Text
  , sirMessage             :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriScriptResult

runSriInvoiceScript :: SriScriptRequest -> IO (Either Text SriScriptResult)
runSriInvoiceScript _ =
  pure (Left "SRI invoice script is not configured in this build")
