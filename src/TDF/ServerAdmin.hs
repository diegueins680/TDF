{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerAdmin
  ( adminServer
  ) where

import           Control.Monad          (unless, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime)
import           Database.Persist       ( (==.), (!=.)
                                        , (=.)
                                        , Entity(..)
                                        , Key
                                        , SelectOpt(..)
                                        , selectFirst
                                        , selectList
                                        , update
                                        , getEntity
                                        , getJustEntity
                                        , insert
                                        )
import           Database.Persist.Sql   (SqlPersistT, runSqlPool)
import           Servant
import           Web.PathPieces         (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Admin          (AdminAPI)
import           TDF.API.Types          ( DropdownOptionCreate(..)
                                        , DropdownOptionDTO(..)
                                        , DropdownOptionUpdate(..)
                                        )
import           TDF.Auth               (AuthedUser, ModuleAccess(..), hasModuleAccess)
import           TDF.DB                 (Env(..))
import           TDF.ModelsExtra (DropdownOption(..))
import qualified TDF.ModelsExtra as ME
import           TDF.Seed               (seedAll)

adminServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT AdminAPI m
adminServer user = seedHandler :<|> dropdownRouter
  where
    seedHandler = do
      ensureModule ModuleAdmin user
      withPool seedAll
      pure NoContent

    dropdownRouter rawCategory =
           listOptions rawCategory
      :<|> createOption rawCategory
      :<|> updateOption rawCategory

    listOptions rawCategory mIncludeInactive = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          includeInactive = fromMaybe False mIncludeInactive
          filters = [ME.DropdownOptionCategory ==. categoryKey]
                 ++ [ME.DropdownOptionActive ==. True | not includeInactive]
          ordering =
            [ Asc ME.DropdownOptionSortOrder
            , Asc ME.DropdownOptionLabel
            , Asc ME.DropdownOptionValue
            ]
      entities <- withPool $ selectList filters ordering
      pure (map toDTO entities)

    createOption rawCategory DropdownOptionCreate{..} = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          valueTxt    = T.strip docValue
      when (T.null valueTxt) $
        throwError err400 { errBody = "Value is required" }
      let labelValue    = normaliseText docLabel
          sortOrderValue = docSortOrder
          activeValue    = fromMaybe True docActive
      conflict <- withPool $ selectFirst
        [ ME.DropdownOptionCategory ==. categoryKey
        , ME.DropdownOptionValue ==. valueTxt
        ]
        []
      when (isJust conflict) $
        throwError err409 { errBody = "Option already exists for category" }
      now <- liftIO getCurrentTime
      entity <- withPool $ do
        optionId <- insert DropdownOption
          { dropdownOptionCategory  = categoryKey
          , dropdownOptionValue     = valueTxt
          , dropdownOptionLabel     = labelValue
          , dropdownOptionActive    = activeValue
          , dropdownOptionSortOrder = sortOrderValue
          , dropdownOptionCreatedAt = now
          , dropdownOptionUpdatedAt = now
          }
        getJustEntity optionId
      pure (toDTO entity)

    updateOption rawCategory rawId DropdownOptionUpdate{..} = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          valueUpdate = fmap T.strip douValue
      when (maybe False T.null valueUpdate) $
        throwError err400 { errBody = "Value must not be empty" }
      optionId <- parseKey rawId
      mOption <- withPool $ getEntity optionId
      case mOption of
        Nothing -> throwError err404
        Just (Entity key option)
          | dropdownOptionCategory option /= categoryKey -> throwError err404
          | otherwise -> do
              case valueUpdate of
                Nothing -> pure ()
                Just newValue -> do
                  conflict <- withPool $ selectFirst
                    [ ME.DropdownOptionCategory ==. categoryKey
                    , ME.DropdownOptionValue ==. newValue
                    , ME.DropdownOptionId !=. key
                    ]
                    []
                  when (isJust conflict) $
                    throwError err409 { errBody = "Option already exists for category" }
              let labelUpdate = fmap normaliseText douLabel
                  sortOrderUpdate = douSortOrder
                  activeUpdate = douActive
              now <- liftIO getCurrentTime
              let baseUpdates = concat
                    [ maybe [] (\v -> [ME.DropdownOptionValue =. v]) valueUpdate
                    , maybe [] (\lbl -> [ME.DropdownOptionLabel =. lbl]) labelUpdate
                    , maybe [] (\s -> [ME.DropdownOptionSortOrder =. s]) sortOrderUpdate
                    , maybe [] (\flag -> [ME.DropdownOptionActive =. flag]) activeUpdate
                    ]
                  updates = if null baseUpdates
                    then []
                    else baseUpdates ++ [ME.DropdownOptionUpdatedAt =. now]
              entity <- if null updates
                then pure (Entity key option)
                else withPool $ do
                  update key updates
                  getJustEntity key
              pure (toDTO entity)

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

parseKey
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Text
  -> m (Key record)
parseKey raw =
  maybe (throwError err400 { errBody = "Invalid identifier" }) pure (fromPathPiece raw)

normaliseCategory :: Text -> Text
normaliseCategory = T.toLower . T.strip

normaliseText :: Maybe Text -> Maybe Text
normaliseText Nothing = Nothing
normaliseText (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

ensureModule
  :: (MonadError ServerError m)
  => ModuleAccess
  -> AuthedUser
  -> m ()
ensureModule moduleTag user =
  unless (hasModuleAccess moduleTag user) $
    throwError err403 { errBody = "Missing required module access" }

toDTO :: Entity DropdownOption -> DropdownOptionDTO
toDTO (Entity key option) = DropdownOptionDTO
  { optionId  = toPathPiece key
  , category  = dropdownOptionCategory option
  , value     = dropdownOptionValue option
  , label     = dropdownOptionLabel option
  , active    = dropdownOptionActive option
  , sortOrder = dropdownOptionSortOrder option
  }
