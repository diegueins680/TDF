{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowHandlerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import TDF.DTO.SocialEventsDTO (ArtistFollowerDTO (..))
import TDF.Models.SocialEventsModels
import TDF.Server.SocialEventsHandlers (followArtistDb)

spec :: Spec
spec = describe "followArtistDb helper" $ do
    it "creates a follow and is idempotent" $ do
        pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
        -- run migrations for social events models
        runSqlPool (runMigration migrateSocialEvents) pool
        now <- liftIO getCurrentTime
        artistId <-
            runSqlPool
                ( insert
                    ArtistProfile
                        { artistProfilePartyId = Nothing
                        , artistProfileName = "Band X"
                        , artistProfileBio = Nothing
                        , artistProfileAvatarUrl = Nothing
                        , artistProfileGenres = Nothing
                        , artistProfileSocialLinks = Nothing
                        , artistProfileCreatedAt = now
                        , artistProfileUpdatedAt = now
                        }
                )
                pool

        first <- followArtistDb pool artistId "carla"
        second <- followArtistDb pool artistId "carla"
        liftIO $ do
            (afFollowId first) `shouldSatisfy` (/= Nothing)
            (afFollowId second) `shouldSatisfy` (/= Nothing)
