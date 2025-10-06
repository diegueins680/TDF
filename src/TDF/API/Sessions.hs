{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Sessions where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types

type SessionsAPI =
       "sessions"
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] (Page SessionDTO)
  :<|> "sessions" :> ReqBody '[JSON] SessionCreate :> PostCreated '[JSON] SessionDTO
  :<|> "sessions" :> Capture "id" Text :> Get '[JSON] SessionDTO
  :<|> "sessions" :> Capture "id" Text :> ReqBody '[JSON] SessionUpdate :> Patch '[JSON] SessionDTO
  :<|> "sessions" :> "options" :> Get '[JSON] SessionOptionsDTO
