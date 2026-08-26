{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Test.Hspec

import TDF.API.Types (RolePayload (..))
import qualified TDF.DTO.SocialEventsDTO as SocialEvents

spec :: Spec
spec = do
  describe "RolePayload FromJSON" $ do
    it "parses raw string payloads" $
        decodeRole "\"Engineer\"" `shouldBe` Right (RolePayload "Engineer")

    it "parses object payloads that provide the role field" $
        decodeRole "{\"role\":\"Teacher\"}" `shouldBe` Right (RolePayload "Teacher")

    it "parses object payloads that provide a value field" $
        decodeRole "{\"value\":\"Artist\"}" `shouldBe` Right (RolePayload "Artist")

    it "fails when neither role nor value is present" $
        decodeRole "{}" `shouldSatisfy` isLeft

  describe "social event live broadcast request FromJSON" $ do
    it "accepts canonical live broadcast create payloads and trims optional fields" $ do
      decodeEventLiveBroadcastCreate
        "{\"elbCreateArtistId\":\"42\",\"elbCreateArtistName\":\" Demo \",\"elbCreateBroadcasterName\":\" Cuco \",\"elbCreateBroadcasterPartyId\":\" 7 \",\"elbCreateTitle\":\" Front row \",\"elbCreateDescription\":\" Coro final \",\"elbCreateQuality\":\" 720p \"}"
        `shouldBe` Right
          ( SocialEvents.EventLiveBroadcastCreateDTO
              "42"
              (Just "Demo")
              (Just "Cuco")
              (Just "7")
              (Just "Front row")
              (Just "Coro final")
              (Just "720p")
          )

    it "accepts omitted optional live broadcast fields" $ do
      decodeEventLiveBroadcastCreate "{\"elbCreateArtistId\":\"42\"}"
        `shouldBe` Right
          ( SocialEvents.EventLiveBroadcastCreateDTO
              "42"
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
          )
      decodeEventLiveBroadcastEnd "{}"
        `shouldBe` Right (SocialEvents.EventLiveBroadcastEndDTO Nothing)
      decodeEventLiveBroadcastHeartbeat "{}"
        `shouldBe` Right (SocialEvents.EventLiveBroadcastHeartbeatDTO Nothing)

    it "rejects typoed live broadcast request fields" $ do
      decodeEventLiveBroadcastCreate "{\"elbCreateArtistId\":\"42\",\"artistId\":\"typo\"}"
        `shouldSatisfy` isLeft
      decodeEventLiveBroadcastEnd "{\"elbEndBroadcasterPartyId\":\"7\",\"broadcastId\":\"typo\"}"
        `shouldSatisfy` isLeft
      decodeEventLiveBroadcastHeartbeat "{\"elbhViewerDelta\":1,\"viewerCount\":2}"
        `shouldSatisfy` isLeft
  where
    decodeRole :: BL8.ByteString -> Either String RolePayload
    decodeRole = eitherDecode
    decodeEventLiveBroadcastCreate :: BL8.ByteString -> Either String SocialEvents.EventLiveBroadcastCreateDTO
    decodeEventLiveBroadcastCreate = eitherDecode
    decodeEventLiveBroadcastEnd :: BL8.ByteString -> Either String SocialEvents.EventLiveBroadcastEndDTO
    decodeEventLiveBroadcastEnd = eitherDecode
    decodeEventLiveBroadcastHeartbeat :: BL8.ByteString -> Either String SocialEvents.EventLiveBroadcastHeartbeatDTO
    decodeEventLiveBroadcastHeartbeat = eitherDecode
    isLeft (Left _) = True
    isLeft (Right _) = False
