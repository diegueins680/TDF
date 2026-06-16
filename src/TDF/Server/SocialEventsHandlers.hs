{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  , normalizeBudgetLineType
  , normalizeEventStatus
  , normalizeEventType
  , normalizeFinanceDirection
  , normalizeFinanceEntryStatus
  , normalizeFinanceSource
  , normalizeInvitationStatus
  , normalizeTicketOrderStatus
  , normalizeTicketStatus
  , parseInvitationIdsEither
  , followArtistDb
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Text.Read (readMaybe)
import           Data.Int (Int64)
import           Data.Time (getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Maybe (isNothing, catMaybes, fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import           Control.Monad (filterM, forM, forM_, when)

import           Servant

-- Pull in full Persistent surface so TH-generated field constructors (EventRsvpEventId, SocialEventStartTime, etc.)
-- are available for filters/updates.
import           Database.Persist
import           Database.Persist.Sql (ConnectionPool, fromSqlKey, runSqlPool, toSqlKey)
import           System.Environment (lookupEnv)

import           TDF.API.SocialEventsAPI
import           TDF.Auth (AuthedUser(..))
import           TDF.DTO.SocialEventsDTO
  ( ArtistDTO(..)
  , ArtistFollowRequest(..)
  , ArtistFollowerDTO(..)
  , ArtistSocialLinksDTO(..)
  , EventDTO(..)
  , EventLiveBroadcastCreateDTO(..)
  , EventLiveBroadcastDTO(..)
  , EventLiveBroadcastEndDTO(..)
  , EventLiveBroadcastHeartbeatDTO(..)
  , InvitationDTO(..)
  , RsvpDTO(..)
  , VenueDTO(..)
  )
import           TDF.DB (Env(..))
import           TDF.ServerRadio
  ( resolveRadioTransmissionEnvBase
  , validateRadioTransmissionIngestBase
  , validateRadioTransmissionPublicBase
  , validateRadioTransmissionWhipBase
  )
import           TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueName)
import qualified TDF.Models.SocialEventsModels as SM

type AppM = ReaderT Env Handler

decodeSocialLinks :: Maybe T.Text -> Maybe ArtistSocialLinksDTO
decodeSocialLinks mTxt = do
  txt <- mTxt
  Aeson.decodeStrict (TE.encodeUtf8 txt)

encodeSocialLinks :: Maybe ArtistSocialLinksDTO -> Maybe T.Text
encodeSocialLinks mLinks =
  fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) mLinks

socialEventsServer :: AuthedUser -> ServerT SocialEventsAPI AppM
socialEventsServer user = eventsServer
               :<|> venuesServer
               :<|> artistsServer
               :<|> rsvpsServer
               :<|> invitationsServer
               :<|> liveBroadcastsServer
  where
    -- Events
    eventsServer :: ServerT EventsRoutes AppM
    eventsServer = listEvents
               :<|> createEvent
               :<|> getEvent
               :<|> updateEvent
               :<|> deleteEvent

    listEvents :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [EventDTO]
    listEvents mCity mStartAfter mArtistId mVenueId = do
      Env{..} <- ask
      let startFilter = case mStartAfter of
            Nothing -> []
            Just raw -> case iso8601ParseM (T.unpack raw) of
              Just t  -> [SocialEventStartTime >=. t]
              Nothing -> []
      cityFilter <- case fmap T.strip mCity of
        Nothing -> pure []
        Just "" -> pure []
        Just cityTxt -> do
          venueRows <- liftIO $ runSqlPool (selectList [VenueCity ==. Just cityTxt] []) envPool
          let ids = map entityKey venueRows
          if null ids
            then pure [SocialEventId ==. toSqlKey 0] -- force empty result set
            else pure [SocialEventVenueId <-. map Just ids]
      venueFilter <- case fmap T.strip mVenueId of
        Nothing -> pure []
        Just "" -> pure []
        Just raw -> case readMaybe (T.unpack raw) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure [SocialEventVenueId ==. Just (toSqlKey vnum)]
      artistFilter <- case fmap T.strip mArtistId of
        Nothing -> pure []
        Just "" -> pure []
        Just raw -> do
          artistKey <- parseArtistId raw
          artistLinks <- liftIO $ runSqlPool (selectList [EventArtistArtistId ==. artistKey] []) envPool
          let eventIds = map (eventArtistEventId . entityVal) artistLinks
          if null eventIds
            then pure [SocialEventId ==. toSqlKey 0]
            else pure [SocialEventId <-. eventIds]
      let filters = startFilter ++ cityFilter ++ venueFilter ++ artistFilter
      rows <- liftIO $ runSqlPool (selectList filters [Desc SocialEventStartTime, LimitTo 200]) envPool
      forM rows $ \(Entity eid e) -> do
        artistLinks <- liftIO $ runSqlPool (selectList [EventArtistEventId ==. eid] []) envPool
        artists <- forM artistLinks $ \(Entity _ link) -> do
          mArtist <- liftIO $ runSqlPool (get (eventArtistArtistId link)) envPool
          pure $ case mArtist of
            Nothing -> ArtistDTO
              { artistId = Nothing
              , artistName = "(unknown)"
              , artistGenres = []
              , artistBio = Nothing
              , artistAvatarUrl = Nothing
              , artistSocialLinks = Nothing
              }
            Just a -> ArtistDTO 
              { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
              , artistName = artistProfileName a
              , artistGenres = maybe [] id (artistProfileGenres a)
              , artistBio = artistProfileBio a
              , artistAvatarUrl = artistProfileAvatarUrl a
              , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
              }
        pure EventDTO
          { eventId = Just (T.pack (show (fromSqlKey eid)))
          , eventTitle = socialEventTitle e
          , eventDescription = socialEventDescription e
          , eventStart = socialEventStartTime e
          , eventEnd = socialEventEndTime e
          , eventVenueId = fmap (T.pack . show . fromSqlKey) (socialEventVenueId e)
          , eventPriceCents = socialEventPriceCents e
          , eventCapacity = socialEventCapacity e
          , eventArtists = artists
          }

    createEvent :: EventDTO -> AppM EventDTO
    createEvent dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure (Just (toSqlKey vnum))
      key <- liftIO $ runSqlPool (insert SocialEvent
        { socialEventOrganizerPartyId = Nothing
        , socialEventTitle = eventTitle dto
        , socialEventDescription = eventDescription dto
        , socialEventVenueId = mVenueKey
        , socialEventStartTime = eventStart dto
        , socialEventEndTime = eventEnd dto
        , socialEventPriceCents = eventPriceCents dto
        , socialEventCapacity = eventCapacity dto
        , socialEventMetadata = Nothing
        , socialEventCreatedAt = now
        , socialEventUpdatedAt = now
        }) envPool
      let artists = eventArtists dto
      liftIO $ runSqlPool
        (forM_ artists $ \a ->
           case artistId a of
             Nothing -> pure ()
             Just atxt -> case readMaybe (T.unpack atxt) :: Maybe Int64 of
               Nothing -> pure ()
               Just anum -> insert_ (EventArtist key (toSqlKey anum) Nothing)
        )
        envPool
      let createdDto = dto { eventId = Just (T.pack (show (fromSqlKey key))) }
      pure createdDto

    getEvent :: T.Text -> AppM EventDTO
    getEvent rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mEnt <- liftIO $ runSqlPool (get key) envPool
          case mEnt of
            Nothing -> throwError err404 { errBody = "Event not found" }
            Just e  -> do
              artistLinks <- liftIO $ runSqlPool (selectList [EventArtistEventId ==. key] []) envPool
              artists <- forM artistLinks $ \(Entity _ link) -> do
                mArtist <- liftIO $ runSqlPool (get (eventArtistArtistId link)) envPool
                pure $ case mArtist of
                  Nothing -> ArtistDTO
                    { artistId = Nothing
                    , artistName = "(unknown)"
                    , artistGenres = []
                    , artistBio = Nothing
                    , artistAvatarUrl = Nothing
                    , artistSocialLinks = Nothing
                    }
                  Just a -> ArtistDTO 
                    { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
                    , artistName = artistProfileName a
                    , artistGenres = maybe [] id (artistProfileGenres a)
                    , artistBio = artistProfileBio a
                    , artistAvatarUrl = artistProfileAvatarUrl a
                    , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
                    }
              pure $ EventDTO
                { eventId = Just (T.pack (show num))
                , eventTitle = socialEventTitle e
                , eventDescription = socialEventDescription e
                , eventStart = socialEventStartTime e
                , eventEnd = socialEventEndTime e
                , eventVenueId = fmap (T.pack . show . fromSqlKey) (socialEventVenueId e)
                , eventPriceCents = socialEventPriceCents e
                , eventCapacity = socialEventCapacity e
                , eventArtists = artists
                }

    updateEvent :: T.Text -> EventDTO -> AppM EventDTO
    updateEvent rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Event not found" }
          when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
          when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
          mVenueKey <- case eventVenueId dto of
            Nothing -> pure Nothing
            Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
              Nothing -> throwError err400 { errBody = "Invalid venue id" }
              Just vnum -> pure (Just (toSqlKey vnum))
          liftIO $ runSqlPool (update key
            [ SocialEventTitle =. eventTitle dto
            , SocialEventDescription =. eventDescription dto
            , SocialEventVenueId =. mVenueKey
            , SocialEventStartTime =. eventStart dto
            , SocialEventEndTime =. eventEnd dto
            , SocialEventPriceCents =. eventPriceCents dto
            , SocialEventCapacity =. eventCapacity dto
            , SocialEventUpdatedAt =. now
            ]) envPool
          liftIO $ runSqlPool (deleteWhere [EventArtistEventId ==. key]) envPool
          let artists = eventArtists dto
          liftIO $ runSqlPool
            (forM_ artists $ \a ->
               case artistId a of
                 Nothing -> pure ()
                 Just atxt -> case readMaybe (T.unpack atxt) :: Maybe Int64 of
                   Nothing -> pure ()
                   Just anum -> insert_ (EventArtist key (toSqlKey anum) Nothing)
            )
            envPool
          pure (dto { eventId = Just rawId })

    deleteEvent :: T.Text -> AppM NoContent
    deleteEvent rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Event not found" }
          liftIO $ runSqlPool
            (do
              deleteWhere [EventArtistEventId ==. key]
              deleteWhere [EventRsvpEventId ==. key]
              deleteWhere [EventInvitationEventId ==. key]
              delete key
            )
            envPool
          pure NoContent

    -- Venues
    venuesServer :: ServerT VenuesRoutes AppM
    venuesServer = listVenues
               :<|> createVenue
               :<|> getVenue
               :<|> updateVenue

    listVenues :: Maybe T.Text -> Maybe T.Text -> AppM [VenueDTO]
    listVenues mCity _mNear = do
      Env{..} <- ask
      let filters = case mCity of
                      Just c | not (T.null (T.strip c)) -> [VenueCity ==. Just (T.strip c)]
                      _ -> []
      rows <- liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo 200]) envPool
      pure $ map (\(Entity vid v) -> VenueDTO
        { venueId = Just (T.pack (show (fromSqlKey vid)))
        , venueName = SM.venueName v
        , venueAddress = SM.venueAddress v
        , venueCity = SM.venueCity v
        , venueCountry = SM.venueCountry v
        , venueLat = venueLatitude v
        , venueLng = venueLongitude v
        , venueCapacity = SM.venueCapacity v
        , venueContact = SM.venueContact v
        }) rows

    createVenue :: VenueDTO -> AppM VenueDTO
    createVenue dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      key <- liftIO $ runSqlPool (insert Venue
        { venueName = venueName dto
        , venueAddress = venueAddress dto
        , venueCity = venueCity dto
        , venueCountry = venueCountry dto
        , venueLatitude = venueLat dto
        , venueLongitude = venueLng dto
        , venueCapacity = venueCapacity dto
        , venueContact = venueContact dto
        , venueCreatedAt = now
        , venueUpdatedAt = now
        }) envPool
      let created = dto { venueId = Just (T.pack (show (fromSqlKey key))) }
      pure created

    getVenue :: T.Text -> AppM VenueDTO
    getVenue rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid venue id" }
        Just num -> do
          let key = toSqlKey num :: VenueId
          mEnt <- liftIO $ runSqlPool (get key) envPool
          case mEnt of
            Nothing -> throwError err404 { errBody = "Venue not found" }
            Just v -> pure VenueDTO
              { venueId = Just (T.pack (show num))
              , venueName = SM.venueName v
              , venueAddress = SM.venueAddress v
              , venueCity = SM.venueCity v
              , venueCountry = SM.venueCountry v
              , venueLat = venueLatitude v
              , venueLng = venueLongitude v
              , venueCapacity = SM.venueCapacity v
              , venueContact = SM.venueContact v
              }

    updateVenue :: T.Text -> VenueDTO -> AppM VenueDTO
    updateVenue rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid venue id" }
        Just num -> do
          let key = toSqlKey num :: VenueId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Venue not found" }
          liftIO $ runSqlPool (update key
            [ VenueName =. venueName dto
            , VenueAddress =. venueAddress dto
            , VenueCity =. venueCity dto
            , VenueCountry =. venueCountry dto
            , VenueLatitude =. venueLat dto
            , VenueLongitude =. venueLng dto
            , VenueCapacity =. venueCapacity dto
            , VenueContact =. venueContact dto
            , VenueUpdatedAt =. now
            ]) envPool
          pure (dto { venueId = Just rawId })

    -- Artists
    artistsServer :: ServerT ArtistsRoutes AppM
    artistsServer = listArtists
               :<|> createArtist
               :<|> getArtist
               :<|> updateArtist
               :<|> listArtistFollowers
               :<|> followArtist
               :<|> unfollowArtist

    listArtists :: Maybe T.Text -> Maybe T.Text -> AppM [ArtistDTO]
    listArtists mNameFilter mGenreFilter = do
      Env{..} <- ask
      let nameFilter = normalizeFilter mNameFilter
          genreFilter = normalizeFilter mGenreFilter
      rows <- liftIO $ runSqlPool (selectList [] [Desc ArtistProfileCreatedAt, LimitTo 500]) envPool
      artists <- forM rows $ \(Entity aid a) -> do
        genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. aid] []) envPool
        let genreList = map (artistGenreGenre . entityVal) genres
        let nameMatches = case nameFilter of
              Nothing -> True
              Just name -> T.isInfixOf name (T.toCaseFold (artistProfileName a))
        let genreMatches = case genreFilter of
              Nothing -> True
              Just genre -> any ((== genre) . T.toCaseFold) genreList
        pure $ if nameMatches && genreMatches
          then Just ArtistDTO
            { artistId = Just (T.pack (show (fromSqlKey aid)))
            , artistName = artistProfileName a
            , artistGenres = genreList
            , artistBio = artistProfileBio a
            , artistAvatarUrl = artistProfileAvatarUrl a
            , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
            }
          else Nothing
      pure (catMaybes artists)

    normalizeFilter :: Maybe T.Text -> Maybe T.Text
    normalizeFilter mVal =
      case fmap (T.toCaseFold . T.strip) mVal of
        Nothing -> Nothing
        Just t | T.null t -> Nothing
        Just t -> Just t

    listArtistFollowers :: T.Text -> AppM [ArtistFollowerDTO]
    listArtistFollowers artistIdStr = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      rows <- liftIO $ runSqlPool
        (selectList [ArtistFollowArtistId ==. artistKey] [Desc ArtistFollowCreatedAt])
        envPool
      let artistIdTxt = T.pack (show (fromSqlKey artistKey))
      pure $ map (\(Entity _ follow) ->
        ArtistFollowerDTO
          { afFollowId = Just (renderFollowId artistKey (artistFollowFollowerPartyId follow))
          , afArtistId = Just artistIdTxt
          , afFollowerPartyId = artistFollowFollowerPartyId follow
          , afCreatedAt = Just (artistFollowCreatedAt follow)
          }) rows
    createArtist :: ArtistDTO -> AppM ArtistDTO
    createArtist dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      key <- liftIO $ runSqlPool (insert ArtistProfile
        { artistProfilePartyId = Nothing
        , artistProfileName = artistName dto
        , artistProfileBio = artistBio dto
        , artistProfileAvatarUrl = artistAvatarUrl dto
        , artistProfileGenres = Just (artistGenres dto)
        , artistProfileSocialLinks = encodeSocialLinks (artistSocialLinks dto)
        , artistProfileCreatedAt = now
        , artistProfileUpdatedAt = now
        }) envPool
      let genreList = artistGenres dto
      liftIO $ runSqlPool
        (forM_ genreList $ \g ->
           insert_ ArtistGenre
             { artistGenreArtistId = key
             , artistGenreGenre = g
             }
        )
        envPool
      pure ArtistDTO
        { artistId = Just (T.pack (show (fromSqlKey key)))
        , artistName = artistName dto
        , artistGenres = genreList
        , artistBio = artistBio dto
        , artistAvatarUrl = artistAvatarUrl dto
        , artistSocialLinks = artistSocialLinks dto
        }

    getArtist :: T.Text -> AppM ArtistDTO
    getArtist idStr = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip idStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> do
          let key = toSqlKey num :: ArtistProfileId
          mArtist <- liftIO $ runSqlPool (get key) envPool
          case mArtist of
            Nothing -> throwError err404 { errBody = "Artist not found" }
            Just a -> do
              genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. key] []) envPool
              pure ArtistDTO
                { artistId = Just (T.strip idStr)
                , artistName = artistProfileName a
                , artistGenres = map (artistGenreGenre . entityVal) genres
                , artistBio = artistProfileBio a
                , artistAvatarUrl = artistProfileAvatarUrl a
                , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
                }

    updateArtist :: T.Text -> ArtistDTO -> AppM ArtistDTO
    updateArtist idStr dto = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip idStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> do
          let key = toSqlKey num :: ArtistProfileId
          now <- liftIO getCurrentTime
          liftIO $ runSqlPool (update key [ArtistProfileName =. artistName dto
                                          , ArtistProfileBio =. artistBio dto
                                          , ArtistProfileAvatarUrl =. artistAvatarUrl dto
                                          , ArtistProfileGenres =. Just (artistGenres dto)
                                          , ArtistProfileSocialLinks =. encodeSocialLinks (artistSocialLinks dto)
                                          , ArtistProfileUpdatedAt =. now
                                          ]) envPool
          -- Update genres
          liftIO $ runSqlPool (deleteWhere [ArtistGenreArtistId ==. key]) envPool
          liftIO $ runSqlPool
            (forM_ (artistGenres dto) $ \g ->
               insert_ ArtistGenre
                 { artistGenreArtistId = key
                 , artistGenreGenre = g
                 }
            )
            envPool
          pure dto { artistId = Just (T.strip idStr) }

    followArtist :: T.Text -> ArtistFollowRequest -> AppM ArtistFollowerDTO
    followArtist artistIdStr ArtistFollowRequest{..} = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      let followerParty = T.strip afrFollowerPartyId
      when (T.null followerParty) $ throwError err400 { errBody = "followerPartyId is required" }
      liftIO $ followArtistDb envPool artistKey followerParty

    unfollowArtist :: T.Text -> Maybe T.Text -> AppM NoContent
    unfollowArtist artistIdStr mFollower = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      followerParty <- case fmap T.strip mFollower of
        Nothing -> throwError err400 { errBody = "follower query param is required" }
        Just t | T.null t -> throwError err400 { errBody = "follower query param is required" }
        Just t -> pure t
      liftIO $ runSqlPool
        (deleteWhere [ArtistFollowArtistId ==. artistKey, ArtistFollowFollowerPartyId ==. followerParty])
        envPool
      pure NoContent

    -- RSVPs
    rsvpsServer :: ServerT RsvpRoutes AppM
    rsvpsServer = listRsvps :<|> createRsvp

    listRsvps :: T.Text -> AppM [RsvpDTO]
    listRsvps eventIdStr = do
      Env{..} <- ask
      case readMaybe (T.unpack eventIdStr) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          rsvpRows <- liftIO $ runSqlPool (selectList [EventRsvpEventId ==. eventKey] []) envPool
          pure $ map (\(Entity rid rsvp) -> RsvpDTO
            { rsvpId = Just (T.pack (show (fromSqlKey rid)))
            , rsvpEventId = eventIdStr
            , rsvpPartyId = eventRsvpPartyId rsvp
            , rsvpStatus = eventRsvpStatus rsvp
            , rsvpCreatedAt = Just (eventRsvpCreatedAt rsvp)
            }) rsvpRows

    createRsvp :: T.Text -> RsvpDTO -> AppM RsvpDTO
    createRsvp eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack eventIdStr) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          -- Verify event exists
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          
          -- Check if RSVP already exists for this party/event
          existingRsvps <- liftIO $ runSqlPool 
            (selectList [EventRsvpEventId ==. eventKey, EventRsvpPartyId ==. rsvpPartyId dto] [])
            envPool
          
          key <- case existingRsvps of
            [] -> do
              -- Create new RSVP
              liftIO $ runSqlPool (insert EventRsvp
                { eventRsvpEventId = eventKey
                , eventRsvpPartyId = rsvpPartyId dto
                , eventRsvpStatus = rsvpStatus dto
                , eventRsvpMetadata = Nothing
                , eventRsvpCreatedAt = now
                , eventRsvpUpdatedAt = now
                }) envPool
            (Entity existingKey _ : _) -> do
              -- Update existing RSVP
              liftIO $ runSqlPool (update existingKey
                [EventRsvpStatus =. rsvpStatus dto
                , EventRsvpUpdatedAt =. now
                ]) envPool
              pure existingKey
          
          pure dto { rsvpId = Just (T.pack (show (fromSqlKey key))) }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer eventIdStr =
      listInvitations eventIdStr
        :<|> createInvitation eventIdStr
        :<|> updateInvitation eventIdStr

    listInvitations :: T.Text -> AppM [InvitationDTO]
    listInvitations eventIdStr = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          rows <- liftIO $ runSqlPool (selectList [EventInvitationEventId ==. eventKey] [Desc EventInvitationCreatedAt]) envPool
          pure $ map (\(Entity iid inv) ->
            InvitationDTO
              { invitationId = Just (T.pack (show (fromSqlKey iid)))
              , invitationEventId = Just (T.strip eventIdStr)
              , invitationFromPartyId = eventInvitationFromPartyId inv
              , invitationToPartyId = maybe "" id (eventInvitationToPartyId inv)
              , invitationStatus = eventInvitationStatus inv
              , invitationMessage = eventInvitationMessage inv
              , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
              , invitationUpdatedAt = Just (eventInvitationUpdatedAt inv)
              }
            ) rows

    createInvitation :: T.Text -> InvitationDTO -> AppM InvitationDTO
    createInvitation eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          let toParty = T.strip (invitationToPartyId dto)
          when (T.null toParty) $ throwError err400 { errBody = "invitationToPartyId is required" }
          let statusVal = normalizeInvitationStatus (invitationStatus dto)
          key <- liftIO $ runSqlPool (insert EventInvitation
            { eventInvitationEventId = eventKey
            , eventInvitationFromPartyId = fmap T.strip (invitationFromPartyId dto)
            , eventInvitationToPartyId = Just toParty
            , eventInvitationStatus = Just statusVal
            , eventInvitationMessage = invitationMessage dto
            , eventInvitationCreatedAt = now
            , eventInvitationUpdatedAt = now
            }) envPool
          pure InvitationDTO
            { invitationId = Just (T.pack (show (fromSqlKey key)))
            , invitationEventId = Just (T.strip eventIdStr)
            , invitationFromPartyId = invitationFromPartyId dto
            , invitationToPartyId = toParty
            , invitationStatus = Just statusVal
            , invitationMessage = invitationMessage dto
            , invitationCreatedAt = Just now
            , invitationUpdatedAt = Just now
            }

    updateInvitation :: T.Text -> T.Text -> InvitationDTO -> AppM InvitationDTO
    updateInvitation eventIdStr invitationIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, invitationKey) <- parseIds eventIdStr invitationIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      mExisting <- liftIO $ runSqlPool (get invitationKey) envPool
      case mExisting of
        Nothing -> throwError err404 { errBody = "Invitation not found" }
        Just inv -> do
          when (eventInvitationEventId inv /= eventKey) $ throwError err400 { errBody = "Invitation does not belong to this event" }
          let statusVal = normalizeInvitationStatus (invitationStatus dto)
          let messageVal = invitationMessage dto <|> eventInvitationMessage inv
          let newToParty = T.strip (invitationToPartyId dto)
          let toPartyVal = if T.null newToParty then eventInvitationToPartyId inv else Just newToParty
          liftIO $ runSqlPool (update invitationKey
            [ EventInvitationStatus =. Just statusVal
            , EventInvitationMessage =. messageVal
            , EventInvitationToPartyId =. toPartyVal
            , EventInvitationUpdatedAt =. now
            ]) envPool
          pure InvitationDTO
            { invitationId = Just (T.pack (show (fromSqlKey invitationKey)))
            , invitationEventId = Just (T.strip eventIdStr)
            , invitationFromPartyId = eventInvitationFromPartyId inv
            , invitationToPartyId = maybe "" id toPartyVal
            , invitationStatus = Just statusVal
            , invitationMessage = messageVal
            , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
            , invitationUpdatedAt = Just now
            }

    parseIds :: T.Text -> T.Text -> AppM (SocialEventId, EventInvitationId)
    parseIds eventIdStr invitationIdStr =
      case parseInvitationIdsEither eventIdStr invitationIdStr of
        Right ids -> pure ids
        Left e -> throwError e

    parseArtistId :: T.Text -> AppM ArtistProfileId
    parseArtistId artistIdStr =
      case readMaybe (T.unpack (T.strip artistIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> pure (toSqlKey num)

    -- Fanclub live broadcasts
    liveBroadcastsServer :: ServerT LiveBroadcastsRoutes AppM
    liveBroadcastsServer =
      listLiveBroadcasts
        :<|> createLiveBroadcast
        :<|> heartbeatLiveBroadcast
        :<|> endLiveBroadcast

    currentPartyIdText :: T.Text
    currentPartyIdText = T.pack (show (fromSqlKey (auPartyId user)))

    listLiveBroadcasts :: T.Text -> AppM [EventLiveBroadcastDTO]
    listLiveBroadcasts eventIdStr = do
      Env{..} <- ask
      eventKey <- parseEventId eventIdStr
      requireEventExists envPool eventKey
      rows <- liftIO $ runSqlPool
        (selectList [EventLiveBroadcastEventId ==. eventKey] [Desc EventLiveBroadcastStartedAt])
        envPool
      visible <- liftIO $
        filterM
          (\(Entity _ row) -> canAccessLiveBroadcast envPool currentPartyIdText row)
          rows
      liftIO $ mapM (\(Entity bid row) -> liveBroadcastToDTO envPool currentPartyIdText bid row) visible

    createLiveBroadcast :: T.Text -> EventLiveBroadcastCreateDTO -> AppM EventLiveBroadcastDTO
    createLiveBroadcast eventIdStr EventLiveBroadcastCreateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseEventId eventIdStr
      requireEventExists envPool eventKey
      artistKey <- parseArtistId elbCreateArtistId
      requireEventArtist envPool eventKey artistKey
      requireArtistFollower envPool artistKey currentPartyIdText
      validateClientBroadcaster elbCreateBroadcasterPartyId currentPartyIdText
      titleVal <- validateLiveTitle elbCreateTitle
      descriptionVal <- validateLiveDescription elbCreateDescription
      _ <- validateLiveQuality elbCreateQuality
      existing <- liftIO $ runSqlPool
        (selectFirst
          [ EventLiveBroadcastEventId ==. eventKey
          , EventLiveBroadcastBroadcasterPartyId ==. currentPartyIdText
          , EventLiveBroadcastStatus ==. "live"
          ]
          [])
        envPool
      when (not (isNothing existing)) $
        throwError err409 { errBody = "This fan already has an active live broadcast for the event" }
      streamKey <- liftIO (toText <$> nextRandom)
      (playbackUrl, ingestUrl, whipUrl) <- resolveLiveBroadcastStreamEndpoints streamKey
      let broadcasterName = fromMaybe ("Party #" <> currentPartyIdText) elbCreateBroadcasterName
      bid <- liftIO $ runSqlPool
        (insert EventLiveBroadcast
          { eventLiveBroadcastEventId = eventKey
          , eventLiveBroadcastArtistId = artistKey
          , eventLiveBroadcastBroadcasterPartyId = currentPartyIdText
          , eventLiveBroadcastBroadcasterName = broadcasterName
          , eventLiveBroadcastTitle = titleVal
          , eventLiveBroadcastDescription = descriptionVal
          , eventLiveBroadcastStatus = "live"
          , eventLiveBroadcastPlaybackUrl = Just playbackUrl
          , eventLiveBroadcastIngestUrl = Just ingestUrl
          , eventLiveBroadcastWhipUrl = Just whipUrl
          , eventLiveBroadcastStreamKey = Just streamKey
          , eventLiveBroadcastViewerCount = 1
          , eventLiveBroadcastStartedAt = now
          , eventLiveBroadcastEndedAt = Nothing
          , eventLiveBroadcastLastHeartbeatAt = now
          , eventLiveBroadcastCreatedAt = now
          , eventLiveBroadcastUpdatedAt = now
          })
        envPool
      liftIO $ loadLiveBroadcastDTO envPool currentPartyIdText bid

    heartbeatLiveBroadcast ::
      T.Text ->
      T.Text ->
      EventLiveBroadcastHeartbeatDTO ->
      AppM EventLiveBroadcastDTO
    heartbeatLiveBroadcast eventIdStr broadcastIdStr EventLiveBroadcastHeartbeatDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseEventId eventIdStr
      broadcastKey <- parseLiveBroadcastId broadcastIdStr
      broadcastRow <- requireLiveBroadcastForEvent envPool eventKey broadcastKey
      canAccess <- liftIO $ canAccessLiveBroadcast envPool currentPartyIdText broadcastRow
      when (not canAccess) $
        throwError err403 { errBody = "Live broadcast is only available to this artist fanclub" }
      let viewerDelta = max (-1000) (min 1000 (fromMaybe 0 elbhViewerDelta))
          nextViewerCount = max 0 (eventLiveBroadcastViewerCount broadcastRow + viewerDelta)
      liftIO $ runSqlPool
        (update broadcastKey
          [ EventLiveBroadcastViewerCount =. nextViewerCount
          , EventLiveBroadcastLastHeartbeatAt =. now
          , EventLiveBroadcastUpdatedAt =. now
          ])
        envPool
      liftIO $ loadLiveBroadcastDTO envPool currentPartyIdText broadcastKey

    endLiveBroadcast :: T.Text -> T.Text -> EventLiveBroadcastEndDTO -> AppM EventLiveBroadcastDTO
    endLiveBroadcast eventIdStr broadcastIdStr EventLiveBroadcastEndDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseEventId eventIdStr
      broadcastKey <- parseLiveBroadcastId broadcastIdStr
      broadcastRow <- requireLiveBroadcastForEvent envPool eventKey broadcastKey
      validateClientBroadcaster elbEndBroadcasterPartyId currentPartyIdText
      when (eventLiveBroadcastBroadcasterPartyId broadcastRow /= currentPartyIdText) $
        throwError err403 { errBody = "Only the broadcaster can end this live session" }
      liftIO $ runSqlPool
        (update broadcastKey
          [ EventLiveBroadcastStatus =. "ended"
          , EventLiveBroadcastEndedAt =. Just now
          , EventLiveBroadcastLastHeartbeatAt =. now
          , EventLiveBroadcastUpdatedAt =. now
          ])
        envPool
      liftIO $ loadLiveBroadcastDTO envPool currentPartyIdText broadcastKey

    parseEventId :: T.Text -> AppM SocialEventId
    parseEventId eventIdStr =
      case readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> pure (toSqlKey num)

    parseLiveBroadcastId :: T.Text -> AppM EventLiveBroadcastId
    parseLiveBroadcastId broadcastIdStr =
      case readMaybe (T.unpack (T.strip broadcastIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid live broadcast id" }
        Just num -> pure (toSqlKey num)

    requireEventExists :: ConnectionPool -> SocialEventId -> AppM ()
    requireEventExists pool eventKey = do
      mEvent <- liftIO $ runSqlPool (get eventKey) pool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }

    requireEventArtist :: ConnectionPool -> SocialEventId -> ArtistProfileId -> AppM ()
    requireEventArtist pool eventKey artistKey = do
      mArtist <- liftIO $ runSqlPool (get artistKey) pool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      mLink <- liftIO $ runSqlPool (get (EventArtistKey eventKey artistKey)) pool
      when (isNothing mLink) $
        throwError err403 { errBody = "Artist is not in this event lineup" }

    requireArtistFollower :: ConnectionPool -> ArtistProfileId -> T.Text -> AppM ()
    requireArtistFollower pool artistKey partyId = do
      mFollow <- liftIO $ runSqlPool (get (ArtistFollowKey artistKey partyId)) pool
      when (isNothing mFollow) $
        throwError err403 { errBody = "Only followers of this artist can start a fanclub live broadcast" }

    requireLiveBroadcastForEvent ::
      ConnectionPool ->
      SocialEventId ->
      EventLiveBroadcastId ->
      AppM EventLiveBroadcast
    requireLiveBroadcastForEvent pool eventKey broadcastKey = do
      mBroadcast <- liftIO $ runSqlPool (get broadcastKey) pool
      broadcastRow <- maybe (throwError err404 { errBody = "Live broadcast not found" }) pure mBroadcast
      when (eventLiveBroadcastEventId broadcastRow /= eventKey) $
        throwError err400 { errBody = "Live broadcast does not belong to this event" }
      pure broadcastRow

-- | Stable, human-friendly identifier for a follow (artistId + follower id).
renderFollowId :: ArtistProfileId -> T.Text -> T.Text
renderFollowId artistId followerPartyId =
  T.intercalate ":" [T.pack (show (fromSqlKey artistId)), followerPartyId]

-- | Insert or fetch an artist follow while keeping the created timestamp stable.
followArtistDb :: ConnectionPool -> ArtistProfileId -> T.Text -> IO ArtistFollowerDTO
followArtistDb pool artistId followerPartyIdRaw = do
  now <- getCurrentTime
  let followerPartyId = T.strip followerPartyIdRaw
  let followKey = ArtistFollowKey artistId followerPartyId
  existing <- runSqlPool (get followKey) pool
  _ <- case existing of
    Just _ -> pure followKey
    Nothing -> do
      mInserted <- runSqlPool (insertUnique (ArtistFollow artistId followerPartyId now)) pool
      pure (fromMaybe followKey mInserted)
  let createdAtVal = maybe now artistFollowCreatedAt existing
  pure ArtistFollowerDTO
    { afFollowId = Just (renderFollowId artistId followerPartyId)
    , afArtistId = Just (T.pack (show (fromSqlKey artistId)))
    , afFollowerPartyId = followerPartyId
    , afCreatedAt = Just createdAtVal
    }

validateClientBroadcaster :: Maybe T.Text -> T.Text -> AppM ()
validateClientBroadcaster Nothing _ = pure ()
validateClientBroadcaster (Just rawPartyId) currentPartyId =
  let normalized = T.strip rawPartyId
  in when (normalized /= currentPartyId) $
      throwError err403 { errBody = "broadcasterPartyId must match authenticated party" }

validateLiveTitle :: Maybe T.Text -> AppM T.Text
validateLiveTitle mTitle =
  let titleVal = fromMaybe "En vivo desde el evento" (fmap T.strip mTitle)
  in if T.null titleVal
      then throwError err400 { errBody = "Live broadcast title is required" }
      else if T.length titleVal > 120
        then throwError err400 { errBody = "Live broadcast title must be 120 characters or less" }
        else pure titleVal

validateLiveDescription :: Maybe T.Text -> AppM (Maybe T.Text)
validateLiveDescription Nothing = pure Nothing
validateLiveDescription (Just rawDescription) =
  let descriptionVal = T.strip rawDescription
  in if T.null descriptionVal
      then pure Nothing
      else if T.length descriptionVal > 280
        then throwError err400 { errBody = "Live broadcast description must be 280 characters or less" }
        else pure (Just descriptionVal)

validateLiveQuality :: Maybe T.Text -> AppM T.Text
validateLiveQuality Nothing = pure "auto"
validateLiveQuality (Just rawQuality) =
  let qualityVal = T.toLower (T.strip rawQuality)
  in if qualityVal `elem` ["auto", "720p", "480p"]
      then pure qualityVal
      else throwError err400 { errBody = "Live broadcast quality must be one of: auto, 720p, 480p" }

resolveLiveBroadcastStreamEndpoints :: T.Text -> AppM (T.Text, T.Text, T.Text)
resolveLiveBroadcastStreamEndpoints streamKey = do
  mPublicBase <- liftIO (lookupEnv "RADIO_PUBLIC_BASE")
  listenBaseRaw <- either throwError pure $
    resolveRadioTransmissionEnvBase
      "RADIO_PUBLIC_BASE"
      "https://radio.tdfrecords.com/streams"
      mPublicBase
  listenBase <- either throwError pure (validateRadioTransmissionPublicBase listenBaseRaw)
  let fallbackIngest = deriveLiveBroadcastBase listenBase "rtmp" "/live"
      fallbackWhip = deriveLiveBroadcastBase listenBase "https" "/whip"
  mIngestBase <- liftIO (lookupEnv "RADIO_INGEST_BASE")
  mWhipBase <- liftIO (lookupEnv "RADIO_WHIP_BASE")
  ingestBaseRaw <- either throwError pure $
    resolveRadioTransmissionEnvBase "RADIO_INGEST_BASE" fallbackIngest mIngestBase
  whipBaseRaw <- either throwError pure $
    resolveRadioTransmissionEnvBase "RADIO_WHIP_BASE" fallbackWhip mWhipBase
  ingestBase <- either throwError pure (validateRadioTransmissionIngestBase ingestBaseRaw)
  whipBase <- either throwError pure (validateRadioTransmissionWhipBase whipBaseRaw)
  pure
    ( appendLiveBroadcastPath listenBase streamKey
    , appendLiveBroadcastPath ingestBase streamKey
    , appendLiveBroadcastPath whipBase streamKey
    )

appendLiveBroadcastPath :: T.Text -> T.Text -> T.Text
appendLiveBroadcastPath base path =
  T.dropWhileEnd (== '/') base <> "/" <> path

deriveLiveBroadcastBase :: T.Text -> T.Text -> T.Text -> T.Text
deriveLiveBroadcastBase baseUrl newScheme newPath =
  let noScheme = fromMaybe baseUrl (T.stripPrefix "https://" baseUrl <|> T.stripPrefix "http://" baseUrl)
      host = T.takeWhile (/= '/') noScheme
      cleanHost = if T.null host then "localhost" else host
      normalizedPath = if T.isPrefixOf "/" newPath then newPath else "/" <> newPath
  in newScheme <> "://" <> cleanHost <> normalizedPath

liveBroadcastToDTO ::
  ConnectionPool ->
  T.Text ->
  EventLiveBroadcastId ->
  EventLiveBroadcast ->
  IO EventLiveBroadcastDTO
liveBroadcastToDTO pool requesterPartyId broadcastKey broadcastRow =
  runSqlPool
    (do
      mArtist <- get (eventLiveBroadcastArtistId broadcastRow)
      let isBroadcaster = eventLiveBroadcastBroadcasterPartyId broadcastRow == requesterPartyId
      pure EventLiveBroadcastDTO
        { elbId = Just (T.pack (show (fromSqlKey broadcastKey)))
        , elbEventId = Just (T.pack (show (fromSqlKey (eventLiveBroadcastEventId broadcastRow))))
        , elbArtistId = T.pack (show (fromSqlKey (eventLiveBroadcastArtistId broadcastRow)))
        , elbArtistName = maybe "Artista" artistProfileName mArtist
        , elbBroadcasterName = eventLiveBroadcastBroadcasterName broadcastRow
        , elbBroadcasterPartyId = Just (eventLiveBroadcastBroadcasterPartyId broadcastRow)
        , elbTitle = eventLiveBroadcastTitle broadcastRow
        , elbDescription = eventLiveBroadcastDescription broadcastRow
        , elbStatus = eventLiveBroadcastStatus broadcastRow
        , elbPlaybackUrl = eventLiveBroadcastPlaybackUrl broadcastRow
        , elbIngestUrl = if isBroadcaster then eventLiveBroadcastIngestUrl broadcastRow else Nothing
        , elbWhipUrl = if isBroadcaster then eventLiveBroadcastWhipUrl broadcastRow else Nothing
        , elbStreamKey = if isBroadcaster then eventLiveBroadcastStreamKey broadcastRow else Nothing
        , elbViewerCount = eventLiveBroadcastViewerCount broadcastRow
        , elbStartedAt = Just (eventLiveBroadcastStartedAt broadcastRow)
        , elbEndedAt = eventLiveBroadcastEndedAt broadcastRow
        , elbLastHeartbeatAt = Just (eventLiveBroadcastLastHeartbeatAt broadcastRow)
        }
    )
    pool

loadLiveBroadcastDTO :: ConnectionPool -> T.Text -> EventLiveBroadcastId -> IO EventLiveBroadcastDTO
loadLiveBroadcastDTO pool requesterPartyId broadcastKey =
  runSqlPool (get broadcastKey) pool >>= \case
    Nothing -> ioError (userError "Live broadcast not found")
    Just broadcastRow -> liveBroadcastToDTO pool requesterPartyId broadcastKey broadcastRow

canAccessLiveBroadcast :: ConnectionPool -> T.Text -> EventLiveBroadcast -> IO Bool
canAccessLiveBroadcast pool partyId broadcastRow =
  if eventLiveBroadcastBroadcasterPartyId broadcastRow == partyId
    then pure True
    else do
      mFollow <- runSqlPool
        (get (ArtistFollowKey (eventLiveBroadcastArtistId broadcastRow) partyId))
        pool
      pure (not (isNothing mFollow))

-- | Normalize invitation status to a lowercase, non-empty value.
normalizeInvitationStatus :: Maybe T.Text -> T.Text
normalizeInvitationStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Nothing -> "pending"
    Just s | T.null s -> "pending"
    Just s -> s

normalizeTicketOrderStatus :: Maybe T.Text -> T.Text
normalizeTicketOrderStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Just "paid" -> "paid"
    Just "refunded" -> "refunded"
    Just "cancelled" -> "cancelled"
    Just "canceled" -> "cancelled"
    _ -> "pending"

normalizeTicketStatus :: Maybe T.Text -> T.Text
normalizeTicketStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Just "checked_in" -> "checked_in"
    Just "checkedin" -> "checked_in"
    Just "used" -> "checked_in"
    Just "cancelled" -> "cancelled"
    Just "canceled" -> "cancelled"
    Just "refunded" -> "refunded"
    _ -> "issued"

normalizeEventType :: Maybe T.Text -> Maybe T.Text
normalizeEventType mType =
  case fmap (T.toLower . T.strip) mType of
    Just "party" -> Just "party"
    Just "concert" -> Just "concert"
    Just "festival" -> Just "festival"
    Just "conference" -> Just "conference"
    Just "showcase" -> Just "showcase"
    Just "other" -> Just "other"
    _ -> Nothing

normalizeEventStatus :: Maybe T.Text -> Maybe T.Text
normalizeEventStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Just "planning" -> Just "planning"
    Just "announced" -> Just "announced"
    Just "on_sale" -> Just "on_sale"
    Just "live" -> Just "live"
    Just "completed" -> Just "completed"
    Just "cancelled" -> Just "cancelled"
    Just "canceled" -> Just "cancelled"
    _ -> Nothing

normalizeBudgetLineType :: Maybe T.Text -> T.Text
normalizeBudgetLineType mType =
  case fmap (T.toLower . T.strip) mType of
    Just "income" -> "income"
    _ -> "expense"

normalizeFinanceDirection :: Maybe T.Text -> T.Text
normalizeFinanceDirection mDirection =
  case fmap (T.toLower . T.strip) mDirection of
    Just "income" -> "income"
    _ -> "expense"

normalizeFinanceSource :: Maybe T.Text -> T.Text
normalizeFinanceSource mSource =
  case fmap (T.toLower . T.strip) mSource of
    Just "ticket_sale" -> "ticket_sale"
    Just "ticket_refund" -> "ticket_refund"
    Just "sponsorship" -> "sponsorship"
    Just "vendor_payment" -> "vendor_payment"
    Just "merchandise" -> "merchandise"
    Just "operations" -> "operations"
    Just "contract_commitment" -> "contract_commitment"
    Just "contract_payment" -> "contract_payment"
    Just "purchase_order" -> "purchase_order"
    Just "purchase_payment" -> "purchase_payment"
    Just "asset_purchase" -> "asset_purchase"
    Just "liability_loan" -> "liability_loan"
    Just "liability_payment" -> "liability_payment"
    Just "accounts_receivable" -> "accounts_receivable"
    Just "accounts_receivable_collection" -> "accounts_receivable_collection"
    Just "accounts_receivable_settlement" -> "accounts_receivable_collection"
    Just "manual" -> "manual"
    Just "other" -> "other"
    _ -> "manual"

normalizeFinanceEntryStatus :: Maybe T.Text -> T.Text
normalizeFinanceEntryStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Just "draft" -> "draft"
    Just "void" -> "void"
    Just "pending" -> "pending"
    _ -> "posted"

-- | Parse event and invitation ids, returning a typed pair or an HTTP 400 error.
parseInvitationIdsEither :: T.Text -> T.Text -> Either ServerError (SocialEventId, EventInvitationId)
parseInvitationIdsEither eventIdStr invitationIdStr =
  case (readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64, readMaybe (T.unpack (T.strip invitationIdStr)) :: Maybe Int64) of
    (Just e, Just i) -> Right (toSqlKey e, toSqlKey i)
    _ -> Left err400 { errBody = "Invalid event or invitation id" }
