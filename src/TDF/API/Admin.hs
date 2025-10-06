{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Admin where

import           Data.Text     (Text)
import           Servant

import           TDF.API.Types ( DropdownOptionCreate
                                , DropdownOptionDTO
                                , DropdownOptionUpdate
                                )

type DropdownCategoryAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [DropdownOptionDTO]
  :<|> ReqBody '[JSON] DropdownOptionCreate :> PostCreated '[JSON] DropdownOptionDTO
  :<|> Capture "optionId" Text :> ReqBody '[JSON] DropdownOptionUpdate :> Patch '[JSON] DropdownOptionDTO

type AdminAPI =
       "seed" :> Post '[JSON] NoContent
  :<|> "dropdowns" :> Capture "category" Text :> DropdownCategoryAPI
