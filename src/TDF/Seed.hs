{-# LANGUAGE OverloadedStrings #-}
module TDF.Seed where

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           TDF.Models

-- Seed data from Diego's YAML (normalized)
seedAll :: SqlPersistT IO ()
seedAll = do
  now <- liftIO getCurrentTime

  -- Parties: Artists & Teachers
  let artists =
        [ ("Arkabuz", Nothing)
        , ("El Bloque", Nothing)
        , ("Skanka Fe", Nothing)
        , ("Quimika Soul", Nothing)
        , ("Juano Ledesma", Just "Juan Ledesma")
        ]
  mapM_ (\(disp, mlegal) -> do
           _ <- insertUnique $ Party mlegal disp False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now
           pure ()
        ) artists

  let teachers =
        [ ("César Galarza", Nothing)
        , ("Fabricio Alomía", Nothing)
        , ("Juan Ledesma", Nothing)
        ]
  mapM_ (\(disp, mlegal) -> do
           pid <- insert $ Party mlegal disp False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now
           _ <- insertUnique (PartyRole pid Teacher True)
           pure ()
        ) teachers

  -- Service Catalog
  let svc name kind pm rate tax = ServiceCatalog name kind pm rate tax True
  _ <- insertUnique (svc "Recording" Recording Hourly Nothing (Just 1200))
  _ <- insertUnique (svc "Mixing" Mixing PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Mastering" Mastering PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Rehearsal" Rehearsal Hourly (Just (15*100)) (Just 1200))
  _ <- insertUnique (svc "Classes" Classes Package Nothing (Just 1200))
  _ <- insertUnique (svc "Event Production" EventProduction Quote Nothing (Just 1200))

  -- Package Product: Guitar 24h
  _ <- insertUnique $ PackageProduct
        { packageProductName = "Guitar 24h"
        , packageProductServiceKind = Classes
        , packageProductUnitsKind = Hours
        , packageProductUnitsQty = 24
        , packageProductPriceCents = 500 * 100
        , packageProductExpiresDays = Just 120
        , packageProductTransferable = False
        , packageProductRefundPolicy = CreditOnly
        , packageProductActive = True
        }

  -- Resources (rooms)
  let rooms = ["Booth A","Booth B","Booth C","Booth D","Live Room","Control Room","Synth Room","Studio A","Studio B","Rehearsal 1","Classroom"]
  mapM_ (\r -> insertUnique (Resource r (slugify r) Room Nothing True) >> pure ()) rooms

  -- Assets (gear)
  let assets = ["Korg SV1","Prophet 08","Moog Subsequent 37","Gibson Ripper Bass","Model 1","SVT","Tone Hammer"]
  mapM_ (\a -> insertUnique (Asset Nothing a "Instrument" Nothing Nothing Nothing Nothing (Just "Studio") Nothing False Nothing True) >> pure ()) assets

  pure ()

slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-"
