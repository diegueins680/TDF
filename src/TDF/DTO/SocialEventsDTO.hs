{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DTO.SocialEventsDTO
  ( ArtistDTO(..)
  , ArtistSocialLinksDTO(..)
  , ArtistFollowerDTO(..)
  , ArtistFollowRequest(..)
  , VenueDTO(..)
  , EventDTO(..)
  , RsvpDTO(..)
  , InvitationDTO(..)
  , EventLiveBroadcastDTO(..)
  , EventLiveBroadcastCreateDTO(..)
  , EventLiveBroadcastEndDTO(..)
  , EventLiveBroadcastHeartbeatDTO(..)
  ) where

import           Control.Monad (unless)
import           Data.Aeson (FromJSON, ToJSON, Value(Null), withObject, (.:), (.:?), (.=), object, toJSON, parseJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.Text  (Text)
import qualified Data.Text as T
import           Data.Time  (UTCTime)
import           GHC.Generics (Generic)

data ArtistSocialLinksDTO = ArtistSocialLinksDTO
  { aslSpotify    :: Maybe Text
  , aslInstagram  :: Maybe Text
  , aslTwitter    :: Maybe Text
  , aslYoutube    :: Maybe Text
  , aslSoundcloud :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistSocialLinksDTO where
  toJSON ArtistSocialLinksDTO{..} = object
    [ "spotify"    .= aslSpotify
    , "instagram"  .= aslInstagram
    , "twitter"    .= aslTwitter
    , "youtube"    .= aslYoutube
    , "soundcloud" .= aslSoundcloud
    ]

instance FromJSON ArtistSocialLinksDTO where
  parseJSON = withObject "ArtistSocialLinksDTO" $ \o ->
    ArtistSocialLinksDTO
      <$> o .:? "spotify"
      <*> o .:? "instagram"
      <*> o .:? "twitter"
      <*> o .:? "youtube"
      <*> o .:? "soundcloud"

data ArtistDTO = ArtistDTO
  { artistId       :: Maybe Text
  , artistName     :: Text
  , artistGenres   :: [Text]
  , artistBio      :: Maybe Text
  , artistAvatarUrl :: Maybe Text
  , artistSocialLinks :: Maybe ArtistSocialLinksDTO
  } deriving (Show, Eq, Generic)
instance ToJSON ArtistDTO
instance FromJSON ArtistDTO

data ArtistFollowerDTO = ArtistFollowerDTO
  { afFollowId         :: Maybe Text
  , afArtistId         :: Maybe Text
  , afFollowerPartyId  :: Text
  , afCreatedAt        :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowerDTO where
  toJSON ArtistFollowerDTO{..} = object
    [ "followId" .= afFollowId
    , "artistId" .= afArtistId
    , "followerPartyId" .= afFollowerPartyId
    , "createdAt" .= afCreatedAt
    ]

instance FromJSON ArtistFollowerDTO where
  parseJSON = withObject "ArtistFollowerDTO" $ \o ->
    ArtistFollowerDTO
      <$> o .:? "followId"
      <*> o .:? "artistId"
      <*> o .:  "followerPartyId"
      <*> o .:? "createdAt"

data ArtistFollowRequest = ArtistFollowRequest
  { afrFollowerPartyId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowRequest where
  toJSON ArtistFollowRequest{..} = object
    [ "followerPartyId" .= afrFollowerPartyId
    ]

instance FromJSON ArtistFollowRequest where
  parseJSON = withObject "ArtistFollowRequest" $ \o ->
    ArtistFollowRequest
      <$> o .: "followerPartyId"

data VenueDTO = VenueDTO
  { venueId       :: Maybe Text
  , venueName     :: Text
  , venueAddress  :: Maybe Text
  , venueCity     :: Maybe Text
  , venueCountry  :: Maybe Text
  , venueLat      :: Maybe Double
  , venueLng      :: Maybe Double
  , venueCapacity :: Maybe Int
  , venueContact  :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON VenueDTO
instance FromJSON VenueDTO

data EventDTO = EventDTO
  { eventId          :: Maybe Text
  , eventTitle       :: Text
  , eventDescription :: Maybe Text
  , eventStart       :: UTCTime
  , eventEnd         :: UTCTime
  , eventVenueId     :: Maybe Text
  , eventPriceCents  :: Maybe Int
  , eventCapacity    :: Maybe Int
  , eventArtists     :: [ArtistDTO]
  } deriving (Show, Eq, Generic)
instance ToJSON EventDTO
instance FromJSON EventDTO

data RsvpDTO = RsvpDTO
  { rsvpId        :: Maybe Text
  , rsvpEventId   :: Text
  , rsvpPartyId   :: Text
  , rsvpStatus    :: Text  -- "Accepted", "Declined", "Maybe"
  , rsvpCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON RsvpDTO
instance FromJSON RsvpDTO

data InvitationDTO = InvitationDTO
  { invitationId         :: Maybe Text
  , invitationEventId    :: Maybe Text
  , invitationFromPartyId :: Maybe Text
  , invitationToPartyId  :: Text
  , invitationStatus     :: Maybe Text
  , invitationMessage    :: Maybe Text
  , invitationCreatedAt  :: Maybe UTCTime
  , invitationUpdatedAt  :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON InvitationDTO
instance FromJSON InvitationDTO

data EventLiveBroadcastDTO = EventLiveBroadcastDTO
  { elbId                 :: Maybe Text
  , elbEventId            :: Maybe Text
  , elbArtistId           :: Text
  , elbArtistName         :: Text
  , elbBroadcasterName    :: Text
  , elbBroadcasterPartyId :: Maybe Text
  , elbTitle              :: Text
  , elbDescription        :: Maybe Text
  , elbStatus             :: Text
  , elbPlaybackUrl        :: Maybe Text
  , elbIngestUrl          :: Maybe Text
  , elbWhipUrl            :: Maybe Text
  , elbStreamKey          :: Maybe Text
  , elbViewerCount        :: Int
  , elbStartedAt          :: Maybe UTCTime
  , elbEndedAt            :: Maybe UTCTime
  , elbLastHeartbeatAt    :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventLiveBroadcastDTO
instance FromJSON EventLiveBroadcastDTO

data EventLiveBroadcastCreateDTO = EventLiveBroadcastCreateDTO
  { elbCreateArtistId           :: Text
  , elbCreateArtistName         :: Maybe Text
  , elbCreateBroadcasterName    :: Maybe Text
  , elbCreateBroadcasterPartyId :: Maybe Text
  , elbCreateTitle              :: Maybe Text
  , elbCreateDescription        :: Maybe Text
  , elbCreateQuality            :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventLiveBroadcastCreateDTO
instance FromJSON EventLiveBroadcastCreateDTO where
  parseJSON = withObject "EventLiveBroadcastCreateDTO" $ \o -> do
    rejectUnknownKeys
      "EventLiveBroadcastCreateDTO"
      [ "elbCreateArtistId"
      , "elbCreateArtistName"
      , "elbCreateBroadcasterName"
      , "elbCreateBroadcasterPartyId"
      , "elbCreateTitle"
      , "elbCreateDescription"
      , "elbCreateQuality"
      ]
      o
    EventLiveBroadcastCreateDTO
      <$> (o .: "elbCreateArtistId")
      <*> optionalTextField o "elbCreateArtistName"
      <*> optionalTextField o "elbCreateBroadcasterName"
      <*> optionalTextField o "elbCreateBroadcasterPartyId"
      <*> optionalTextField o "elbCreateTitle"
      <*> optionalTextField o "elbCreateDescription"
      <*> optionalTextField o "elbCreateQuality"

data EventLiveBroadcastEndDTO = EventLiveBroadcastEndDTO
  { elbEndBroadcasterPartyId :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventLiveBroadcastEndDTO
instance FromJSON EventLiveBroadcastEndDTO where
  parseJSON = withObject "EventLiveBroadcastEndDTO" $ \o -> do
    rejectUnknownKeys "EventLiveBroadcastEndDTO" ["elbEndBroadcasterPartyId"] o
    EventLiveBroadcastEndDTO
      <$> optionalTextField o "elbEndBroadcasterPartyId"

data EventLiveBroadcastHeartbeatDTO = EventLiveBroadcastHeartbeatDTO
  { elbhViewerDelta :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON EventLiveBroadcastHeartbeatDTO
instance FromJSON EventLiveBroadcastHeartbeatDTO where
  parseJSON = withObject "EventLiveBroadcastHeartbeatDTO" $ \o -> do
    rejectUnknownKeys "EventLiveBroadcastHeartbeatDTO" ["elbhViewerDelta"] o
    EventLiveBroadcastHeartbeatDTO <$> o .:? "elbhViewerDelta"

rejectUnknownKeys :: String -> [Text] -> KeyMap.KeyMap Value -> Parser ()
rejectUnknownKeys label allowedKeys o = do
  let allowed = fmap Key.fromText allowedKeys
      unknown = filter (`notElem` allowed) (KeyMap.keys o)
  unless (null unknown) $
    fail (label <> " contains unexpected fields: " <> show (fmap Key.toString unknown))

normalizeOptionalText :: Maybe Value -> Parser (Maybe Text)
normalizeOptionalText Nothing = pure Nothing
normalizeOptionalText (Just Null) = fail "optional text fields must be omitted instead of null"
normalizeOptionalText (Just value) = do
  raw <- parseJSON value
  let trimmed = T.strip raw
  pure (if T.null trimmed then Nothing else Just trimmed)

optionalTextField :: KeyMap.KeyMap Value -> Text -> Parser (Maybe Text)
optionalTextField o key = normalizeOptionalText (KeyMap.lookup (Key.fromText key) o)
