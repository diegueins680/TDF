{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Client
  ( SendTextResult(..)
  , extractMessageId
  , normalizeGraphApiVersion
  , normalizeWhatsAppAccessToken
  , normalizeWhatsAppMessageBody
  , normalizeWhatsAppPhoneNumberId
  , normalizeWhatsAppRecipientPhone
  , normalizeWhatsAppVerifyToken
  , sendText
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (GeneralCategory(Format), generalCategory, isControl, isSpace)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAuthorization)
import           Network.HTTP.Types.Status (statusCode)

data SendTextResult = SendTextResult
  { sendTextPayload   :: Value
  , sendTextMessageId :: Maybe Text
  } deriving (Show)

sendText :: Manager -> Text -> Text -> Text -> Text -> Text -> IO (Either String SendTextResult)
sendText mgr apiVersion token phoneId to body =
  case normalizeGraphApiVersion apiVersion of
    Left err -> pure (Left err)
    Right version ->
      case normalizeWhatsAppAccessToken token of
        Left err -> pure (Left err)
        Right accessToken ->
          case normalizeWhatsAppPhoneNumberId phoneId of
            Left err -> pure (Left err)
            Right normalizedPhoneId ->
              case normalizeWhatsAppRecipientPhone to of
                Left err -> pure (Left err)
                Right recipientPhone ->
                  case normalizeWhatsAppMessageBody body of
                    Left err -> pure (Left err)
                    Right messageBody -> sendTextRequest mgr version accessToken normalizedPhoneId recipientPhone messageBody

sendTextRequest :: Manager -> Text -> Text -> Text -> Text -> Text -> IO (Either String SendTextResult)
sendTextRequest mgr version accessToken phoneId to body = do
  initReq <-
    parseRequest $
      "https://graph.facebook.com/"
        <> T.unpack version
        <> "/"
        <> T.unpack phoneId
        <> "/messages"
  let payload = object
        [ "messaging_product" .= ("whatsapp" :: Text)
        , "to" .= to
        , "type" .= ("text" :: Text)
        , "text" .= object ["body" .= body]
        ]
      req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , (hAuthorization, BS.pack $ "Bearer " <> T.unpack accessToken)
            ]
        , requestBody = RequestBodyLBS (encode payload)
        }
  res <- try (httpLbs req mgr) :: IO (Either SomeException (Response LBS.ByteString))
  pure $ case res of
    Left e -> Left (show e)
    Right ok ->
      let status = statusCode (responseStatus ok)
          rawBody = responseBody ok
      in case eitherDecode' rawBody of
           Left err ->
             let rendered = TE.decodeUtf8With lenientDecode (LBS.toStrict rawBody)
             in Left ("Failed to decode WhatsApp API response (" <> show status <> "): " <> err <> " | " <> T.unpack rendered)
           Right parsed
             | status >= 200 && status < 300 ->
                 Right SendTextResult
                   { sendTextPayload = parsed
                   , sendTextMessageId = extractMessageId parsed
                   }
             | otherwise ->
                 Left ("HTTP " <> show status <> ": " <> T.unpack (renderValue parsed))

normalizeGraphApiVersion :: Text -> Either String Text
normalizeGraphApiVersion rawVersion
  | T.null version = Right "v20.0"
  | isValidVersion version = Right version
  | otherwise = Left "Invalid WhatsApp Graph API version: expected vMAJOR or vMAJOR.MINOR"
  where
    version = T.toLower (T.strip rawVersion)
    isValidVersion value =
      case T.uncons value of
        Just ('v', rest) ->
          case T.splitOn "." rest of
            [major] -> isPositiveVersionSegment major
            [major, minor] -> isPositiveVersionSegment major && isCanonicalVersionSegment minor
            _ -> False
        _ -> False
    isPositiveVersionSegment value =
      isCanonicalVersionSegment value && value /= "0"
    isCanonicalVersionSegment value =
      not (T.null value)
        && T.all (\ch -> ch >= '0' && ch <= '9') value
        && (value == "0" || not ("0" `T.isPrefixOf` value))

normalizeWhatsAppAccessToken :: Text -> Either String Text
normalizeWhatsAppAccessToken = normalizeHeaderText "WhatsApp access token" 4096

normalizeWhatsAppVerifyToken :: Text -> Either String Text
normalizeWhatsAppVerifyToken = normalizeHeaderText "WhatsApp verify token" 512

normalizeWhatsAppPhoneNumberId :: Text -> Either String Text
normalizeWhatsAppPhoneNumberId rawPhoneId
  | T.null phoneId = Left "Invalid WhatsApp phone number id: id is required"
  | T.length phoneId > 64 = Left "Invalid WhatsApp phone number id: id must be 64 digits or fewer"
  | T.all isAsciiDigit phoneId = Right phoneId
  | otherwise = Left "Invalid WhatsApp phone number id: expected digits only"
  where
    phoneId = T.strip rawPhoneId

normalizeWhatsAppRecipientPhone :: Text -> Either String Text
normalizeWhatsAppRecipientPhone rawPhone =
  let trimmed = T.strip rawPhone
      onlyDigits = T.filter isAsciiDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiDigit trimmed
      allowedPhoneChar ch = isAsciiDigit ch || ch == ' ' || ch `elem` ("+-()." :: String)
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx == 0 && digitIdx == 1
  in if T.null trimmed || T.null onlyDigits
       then Left "Invalid WhatsApp recipient phone: phone is required"
       else if digitCount < 8 || digitCount > 15 || T.any (not . allowedPhoneChar) trimmed || not plusIsValid
         then Left "Invalid WhatsApp recipient phone: expected 8-15 digits with optional leading + and phone separators"
         else Right ("+" <> onlyDigits)

normalizeWhatsAppMessageBody :: Text -> Either String Text
normalizeWhatsAppMessageBody rawBody
  | T.null body = Left "Invalid WhatsApp message body: message is required"
  | T.length body > 4096 = Left "Invalid WhatsApp message body: message must be 4096 characters or fewer"
  | T.any invalidMessageBodyControlChar body =
      Left "Invalid WhatsApp message body: message must not contain unsupported control characters"
  | T.any isHiddenFormattingChar body =
      Left "Invalid WhatsApp message body: message must not contain hidden formatting characters"
  | otherwise = Right body
  where
    body = T.strip rawBody

normalizeHeaderText :: String -> Int -> Text -> Either String Text
normalizeHeaderText label maxLength rawValue
  | T.null value = Left ("Invalid " <> label <> ": token is required")
  | T.length value > maxLength = Left ("Invalid " <> label <> ": token is too long")
  | T.any isUnsafeHeaderChar value = Left ("Invalid " <> label <> ": contains unsafe characters")
  | T.any (not . isVisibleAsciiHeaderChar) value = Left ("Invalid " <> label <> ": must contain visible ASCII characters only")
  | otherwise = Right value
  where
    value = T.strip rawValue

extractMessageId :: Value -> Maybe Text
extractMessageId =
  parseMaybe $
    withObject "SendTextResult" $ \o -> do
      msgs <- o .:? "messages" .!= ([] :: [Value])
      case mapMaybe pullId msgs of
        [msgId] -> pure msgId
        _ -> fail "Expected exactly one WhatsApp message id"
  where
    pullId =
      parseMaybe $
        withObject "WhatsAppMessageId" $ \msg -> do
          rawId <- msg .: "id"
          let msgId = T.strip rawId
          if T.null msgId || T.any isUnsafeHeaderChar msgId
            then fail "Invalid WhatsApp message id"
            else pure msgId

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

isUnsafeHeaderChar :: Char -> Bool
isUnsafeHeaderChar ch = isControl ch || isSpace ch || isHiddenFormattingChar ch

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch = generalCategory ch == Format

isVisibleAsciiHeaderChar :: Char -> Bool
isVisibleAsciiHeaderChar ch = ch >= '!' && ch <= '~'

invalidMessageBodyControlChar :: Char -> Bool
invalidMessageBodyControlChar ch =
  isControl ch && ch `notElem` ("\n\r\t" :: String)

renderValue :: Value -> Text
renderValue = TE.decodeUtf8 . LBS.toStrict . encode
