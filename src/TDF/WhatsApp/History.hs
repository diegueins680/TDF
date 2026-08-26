{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.WhatsApp.History
  ( IncomingWhatsAppRecord(..)
  , OutgoingWhatsAppRecord(..)
  , WhatsAppDeliveryUpdate(..)
  , applyWhatsAppDeliveryUpdate
  , normalizeWhatsAppPhone
  , recordIncomingWhatsAppMessage
  , recordOutgoingWhatsAppMessage
  ) where

import           Control.Applicative ((<|>))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.Persist
  ( Entity(..)
  , Key
  , getBy
  , getJustEntity
  , insert
  , update
  , (=.)
  )
import           Database.Persist.Sql (SqlPersistT)

import           TDF.Models (PartyId)
import qualified TDF.ModelsExtra as ME
import           TDF.WhatsApp.Client (SendTextResult(..))

data IncomingWhatsAppRecord = IncomingWhatsAppRecord
  { iwrExternalId         :: Text
  , iwrSenderId           :: Text
  , iwrSenderName         :: Maybe Text
  , iwrText               :: Text
  , iwrAdExternalId       :: Maybe Text
  , iwrAdName             :: Maybe Text
  , iwrCampaignExternalId :: Maybe Text
  , iwrCampaignName       :: Maybe Text
  , iwrMetadata           :: Maybe Text
  , iwrTransportPayload   :: Maybe Text
  , iwrSource             :: Maybe Text
  } deriving (Show)

data OutgoingWhatsAppRecord = OutgoingWhatsAppRecord
  { owrRecipientPhone    :: Text
  , owrRecipientPartyId  :: Maybe PartyId
  , owrRecipientName     :: Maybe Text
  , owrRecipientEmail    :: Maybe Text
  , owrActorPartyId      :: Maybe PartyId
  , owrBody              :: Text
  , owrSource            :: Maybe Text
  , owrReplyToMessageId  :: Maybe (Key ME.WhatsAppMessage)
  , owrReplyToExternalId :: Maybe Text
  , owrResendOfMessageId :: Maybe (Key ME.WhatsAppMessage)
  , owrMetadata          :: Maybe Text
  } deriving (Show)

data WhatsAppDeliveryUpdate = WhatsAppDeliveryUpdate
  { wduExternalId    :: Text
  , wduStatus        :: Text
  , wduRecipientId   :: Maybe Text
  , wduOccurredAt    :: Maybe UTCTime
  , wduDeliveryError :: Maybe Text
  , wduStatusPayload :: Maybe Text
  } deriving (Show)

normalizeWhatsAppPhone :: Text -> Maybe Text
normalizeWhatsAppPhone raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isAsciiDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiDigit trimmed
      allowedPhoneChar ch =
        isAsciiDigit ch || ch == ' ' || ch `elem` ("+-()." :: String)
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx == 0 && digitIdx == 1
  in if T.null onlyDigits
        || digitCount < 8
        || digitCount > 15
        || T.any (not . allowedPhoneChar) trimmed
        || not plusIsValid
       then Nothing
       else Just ("+" <> onlyDigits)

recordIncomingWhatsAppMessage
  :: UTCTime
  -> IncomingWhatsAppRecord
  -> SqlPersistT IO (Entity ME.WhatsAppMessage)
recordIncomingWhatsAppMessage now IncomingWhatsAppRecord{..} = do
  let externalId = nonEmptyOr ("incoming-" <> T.pack (show now)) iwrExternalId
      senderId = nonEmptyOr "unknown" iwrSenderId
  mExisting <- getBy (ME.UniqueWhatsAppMessage externalId)
  case mExisting of
    Just existing -> pure existing
    Nothing -> do
      key <- insert ME.WhatsAppMessage
        { ME.whatsAppMessageExternalId = externalId
        , ME.whatsAppMessageSenderId = senderId
        , ME.whatsAppMessageSenderName = iwrSenderName
        , ME.whatsAppMessagePartyId = Nothing
        , ME.whatsAppMessageActorPartyId = Nothing
        , ME.whatsAppMessagePhoneE164 = normalizeWhatsAppPhone senderId
        , ME.whatsAppMessageContactEmail = Nothing
        , ME.whatsAppMessageText = Just iwrText
        , ME.whatsAppMessageDirection = "incoming"
        , ME.whatsAppMessageAdExternalId = iwrAdExternalId
        , ME.whatsAppMessageAdName = iwrAdName
        , ME.whatsAppMessageCampaignExternalId = iwrCampaignExternalId
        , ME.whatsAppMessageCampaignName = iwrCampaignName
        , ME.whatsAppMessageMetadata = iwrMetadata
        , ME.whatsAppMessageReplyStatus = "pending"
        , ME.whatsAppMessageHoldReason = Nothing
        , ME.whatsAppMessageHoldRequiredFields = Nothing
        , ME.whatsAppMessageLastAttemptAt = Nothing
        , ME.whatsAppMessageAttemptCount = 0
        , ME.whatsAppMessageRepliedAt = Nothing
        , ME.whatsAppMessageReplyText = Nothing
        , ME.whatsAppMessageReplyError = Nothing
        , ME.whatsAppMessageDeliveryStatus = "received"
        , ME.whatsAppMessageDeliveryUpdatedAt = Nothing
        , ME.whatsAppMessageDeliveryError = Nothing
        , ME.whatsAppMessageTransportPayload = iwrTransportPayload
        , ME.whatsAppMessageStatusPayload = Nothing
        , ME.whatsAppMessageSource = iwrSource
        , ME.whatsAppMessageResendOfMessageId = Nothing
        , ME.whatsAppMessageCreatedAt = now
        }
      getJustEntity key

recordOutgoingWhatsAppMessage
  :: UTCTime
  -> OutgoingWhatsAppRecord
  -> Either Text SendTextResult
  -> SqlPersistT IO (Entity ME.WhatsAppMessage)
recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord{..} sendResult = do
  let providerId = either (const Nothing) sendTextMessageId sendResult
      externalId = fromMaybe ("outgoing-" <> T.pack (show now)) providerId
      deliveryStatus = either (const "failed") (const "sent") sendResult
      replyError = either Just (const Nothing) sendResult
  mExisting <- getBy (ME.UniqueWhatsAppMessage externalId)
  case mExisting of
    Just existing -> pure existing
    Nothing -> do
      key <- insert ME.WhatsAppMessage
        { ME.whatsAppMessageExternalId = externalId
        , ME.whatsAppMessageSenderId = owrRecipientPhone
        , ME.whatsAppMessageSenderName = owrRecipientName
        , ME.whatsAppMessagePartyId = owrRecipientPartyId
        , ME.whatsAppMessageActorPartyId = owrActorPartyId
        , ME.whatsAppMessagePhoneE164 = normalizeWhatsAppPhone owrRecipientPhone
        , ME.whatsAppMessageContactEmail = owrRecipientEmail
        , ME.whatsAppMessageText = Just owrBody
        , ME.whatsAppMessageDirection = "outgoing"
        , ME.whatsAppMessageAdExternalId = Nothing
        , ME.whatsAppMessageAdName = Nothing
        , ME.whatsAppMessageCampaignExternalId = Nothing
        , ME.whatsAppMessageCampaignName = Nothing
        , ME.whatsAppMessageMetadata = owrMetadata
        , ME.whatsAppMessageReplyStatus = deliveryStatus
        , ME.whatsAppMessageHoldReason = Nothing
        , ME.whatsAppMessageHoldRequiredFields = Nothing
        , ME.whatsAppMessageLastAttemptAt = Just now
        , ME.whatsAppMessageAttemptCount = 1
        , ME.whatsAppMessageRepliedAt = Just now
        , ME.whatsAppMessageReplyText = Just owrBody
        , ME.whatsAppMessageReplyError = replyError
        , ME.whatsAppMessageDeliveryStatus = deliveryStatus
        , ME.whatsAppMessageDeliveryUpdatedAt = Just now
        , ME.whatsAppMessageDeliveryError = replyError
        , ME.whatsAppMessageTransportPayload = Nothing
        , ME.whatsAppMessageStatusPayload = Nothing
        , ME.whatsAppMessageSource = owrSource
        , ME.whatsAppMessageResendOfMessageId = owrResendOfMessageId <|> owrReplyToMessageId
        , ME.whatsAppMessageCreatedAt = now
        }
      getJustEntity key

applyWhatsAppDeliveryUpdate
  :: UTCTime
  -> WhatsAppDeliveryUpdate
  -> SqlPersistT IO ()
applyWhatsAppDeliveryUpdate now WhatsAppDeliveryUpdate{..} = do
  mExisting <- getBy (ME.UniqueWhatsAppMessage wduExternalId)
  case mExisting of
    Nothing -> pure ()
    Just (Entity key _) ->
      update key
        [ ME.WhatsAppMessageDeliveryStatus =. wduStatus
        , ME.WhatsAppMessageDeliveryUpdatedAt =. Just (fromMaybe now wduOccurredAt)
        , ME.WhatsAppMessageDeliveryError =. wduDeliveryError
        , ME.WhatsAppMessageStatusPayload =. wduStatusPayload
        ]

nonEmptyOr :: Text -> Text -> Text
nonEmptyOr fallback raw =
  let value = T.strip raw
  in if T.null value then fallback else value

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'
