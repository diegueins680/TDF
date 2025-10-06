{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Types where

import           Data.Aeson   (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.=))
import           Data.Int     (Int64)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

data Page a = Page
  { items    :: [a]
  , page     :: Int
  , pageSize :: Int
  , total    :: Int
  } deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Page a)
instance (FromJSON a) => FromJSON (Page a)

data DropdownOptionDTO = DropdownOptionDTO
  { optionId  :: Text
  , category  :: Text
  , value     :: Text
  , label     :: Maybe Text
  , active    :: Bool
  , sortOrder :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON DropdownOptionDTO
instance FromJSON DropdownOptionDTO

data DropdownOptionCreate = DropdownOptionCreate
  { docValue     :: Text
  , docLabel     :: Maybe Text
  , docSortOrder :: Maybe Int
  , docActive    :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON DropdownOptionCreate
instance FromJSON DropdownOptionCreate

data DropdownOptionUpdate = DropdownOptionUpdate
  { douValue     :: Maybe Text
  , douLabel     :: Maybe (Maybe Text)
  , douSortOrder :: Maybe (Maybe Int)
  , douActive    :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON DropdownOptionUpdate
instance FromJSON DropdownOptionUpdate

data BandOptionsDTO = BandOptionsDTO
  { roles  :: [DropdownOptionDTO]
  , genres :: [DropdownOptionDTO]
  } deriving (Show, Generic)

instance ToJSON BandOptionsDTO
instance FromJSON BandOptionsDTO

data BandChoiceDTO = BandChoiceDTO
  { bandId :: Text
  , name   :: Text
  } deriving (Show, Generic)

instance ToJSON BandChoiceDTO
instance FromJSON BandChoiceDTO

data SessionOptionsDTO = SessionOptionsDTO
  { bands :: [BandChoiceDTO]
  } deriving (Show, Generic)

instance ToJSON SessionOptionsDTO
instance FromJSON SessionOptionsDTO

data AssetDTO = AssetDTO
  { assetId  :: Text
  , name     :: Text
  , category :: Text
  , status   :: Text
  , location :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AssetDTO
instance FromJSON AssetDTO

data AssetCreate = AssetCreate
  { cName     :: Text
  , cCategory :: Text
  } deriving (Show, Generic)

instance ToJSON AssetCreate
instance FromJSON AssetCreate

data AssetUpdate = AssetUpdate
  { uName       :: Maybe Text
  , uCategory   :: Maybe Text
  , uStatus     :: Maybe Text
  , uLocationId :: Maybe Text
  , uNotes      :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AssetUpdate
instance ToJSON AssetUpdate

data RoomDTO = RoomDTO
  { roomId    :: Text
  , rName     :: Text
  , rBookable :: Bool
  } deriving (Show, Generic)

instance ToJSON RoomDTO
instance FromJSON RoomDTO

data RoomCreate = RoomCreate
  { rcName :: Text
  } deriving (Show, Generic)

instance ToJSON RoomCreate
instance FromJSON RoomCreate

data RoomUpdate = RoomUpdate
  { ruName       :: Maybe Text
  , ruIsBookable :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON RoomUpdate
instance FromJSON RoomUpdate

data SessionInputRow = SessionInputRow
  { channelNumber    :: Int
  , trackName        :: Maybe Text
  , instrument       :: Maybe Text
  , micId            :: Maybe Text
  , standId          :: Maybe Text
  , cableId          :: Maybe Text
  , preampId         :: Maybe Text
  , insertOutboardId :: Maybe Text
  , converterChannel :: Maybe Text
  , phantom          :: Maybe Bool
  , polarity         :: Maybe Bool
  , hpf              :: Maybe Bool
  , pad              :: Maybe Bool
  , notes            :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionInputRow
instance FromJSON SessionInputRow

data SessionDTO = SessionDTO
  { sessionId            :: Text
  , sStartAt             :: UTCTime
  , sEndAt               :: UTCTime
  , sStatus              :: Text
  , sBookingRef          :: Maybe Text
  , sBandId              :: Maybe Text
  , sClientPartyRef      :: Maybe Text
  , sService             :: Text
  , sEngineerRef         :: Text
  , sAssistantRef        :: Maybe Text
  , sRoomIds             :: [Text]
  , sSampleRate          :: Maybe Int
  , sBitDepth            :: Maybe Int
  , sDaw                 :: Maybe Text
  , sSessionFolderDriveId:: Maybe Text
  , sNotes               :: Maybe Text
  , sInputListRows       :: [SessionInputRow]
  } deriving (Show, Generic)

instance ToJSON SessionDTO
instance FromJSON SessionDTO

data SessionCreate = SessionCreate
  { scBookingRef          :: Maybe Text
  , scBandId              :: Maybe Text
  , scClientPartyRef      :: Maybe Text
  , scService             :: Text
  , scStartAt             :: UTCTime
  , scEndAt               :: UTCTime
  , scEngineerRef         :: Text
  , scAssistantRef        :: Maybe Text
  , scRoomIds             :: [Text]
  , scSampleRate          :: Maybe Int
  , scBitDepth            :: Maybe Int
  , scDaw                 :: Maybe Text
  , scSessionFolderDriveId:: Maybe Text
  , scNotes               :: Maybe Text
  , scInputListRows       :: Maybe [SessionInputRow]
  , scStatus              :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionCreate
instance FromJSON SessionCreate

data SessionUpdate = SessionUpdate
  { suBookingRef          :: Maybe (Maybe Text)
  , suBandId              :: Maybe (Maybe Text)
  , suClientPartyRef      :: Maybe (Maybe Text)
  , suService             :: Maybe Text
  , suStartAt             :: Maybe UTCTime
  , suEndAt               :: Maybe UTCTime
  , suEngineerRef         :: Maybe Text
  , suAssistantRef        :: Maybe (Maybe Text)
  , suRoomIds             :: Maybe [Text]
  , suSampleRate          :: Maybe (Maybe Int)
  , suBitDepth            :: Maybe (Maybe Int)
  , suDaw                 :: Maybe (Maybe Text)
  , suSessionFolderDriveId:: Maybe (Maybe Text)
  , suNotes               :: Maybe (Maybe Text)
  , suInputListRows       :: Maybe [SessionInputRow]
  , suStatus              :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionUpdate
instance FromJSON SessionUpdate

data BandMemberDTO = BandMemberDTO
  { bmId         :: Text
  , bmPartyId    :: Int64
  , bmPartyName  :: Text
  , bmRole       :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON BandMemberDTO
instance FromJSON BandMemberDTO

data BandDTO = BandDTO
  { bandId        :: Text
  , partyId       :: Int64
  , bName         :: Text
  , bLabelArtist  :: Bool
  , bPrimaryGenre :: Maybe Text
  , bHomeCity     :: Maybe Text
  , bPhotoUrl     :: Maybe Text
  , bContractFlags:: Maybe Text
  , bMembers      :: [BandMemberDTO]
  } deriving (Show, Generic)

instance ToJSON BandDTO
instance FromJSON BandDTO

data BandMemberInput = BandMemberInput
  { bmiPartyId :: Int64
  , bmiRole    :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON BandMemberInput where
  toJSON BandMemberInput{bmiPartyId, bmiRole} = object
    [ "bmPartyId" .= bmiPartyId
    , "bmRole"     .= bmiRole
    ]

instance FromJSON BandMemberInput where
  parseJSON = withObject "BandMemberInput" $ \o ->
    BandMemberInput
      <$> o .:  "bmPartyId"
      <*> o .:? "bmRole"

data BandCreate = BandCreate
  { bcName          :: Text
  , bcLabelArtist   :: Maybe Bool
  , bcPrimaryGenre  :: Maybe Text
  , bcHomeCity      :: Maybe Text
  , bcPhotoUrl      :: Maybe Text
  , bcContractFlags :: Maybe Text
  , bcMembers       :: [BandMemberInput]
  } deriving (Show, Generic)

instance ToJSON BandCreate
instance FromJSON BandCreate
