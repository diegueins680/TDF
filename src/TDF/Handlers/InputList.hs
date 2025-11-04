{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module TDF.Handlers.InputList
  ( InventoryItem
  , InputListEntry
  , listInventoryDB
  , seedInventoryDB
  , seedHQDB
  , fetchSessionInputRowsByIndex
  , fetchSessionInputRowsByKey
  , renderInputListLatex
  , generateInputListPdf
  , sanitizeFileName
  ) where

import           Control.Applicative        ((<|>))
import           Control.Exception          (IOException, catch)
import           Data.Char                  (isAlphaNum)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.ByteString.Lazy       as BL
import           Data.Aeson                 (ToJSON(..), object, (.=))
import           Data.Time                  (UTCTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT)
import           Web.PathPieces             (toPathPiece)
import           System.Directory           (createDirectoryIfMissing, removeFile)
import           System.Exit                (ExitCode(..))
import           System.FilePath            ((</>))
import           System.Process             (readProcessWithExitCode)

import qualified TDF.ModelsExtra            as ME
import           TDF.Seed                   (seedHolgerSession, seedInventoryAssets)

type InventoryItem = ME.Asset
type InputListEntry = ME.InputRow

instance ToJSON (Entity InventoryItem) where
  toJSON (Entity key item) = object
    [ "id"        .= toPathPiece key
    , "name"      .= ME.assetName item
    , "category"  .= ME.assetCategory item
    , "brand"     .= ME.assetBrand item
    , "model"     .= ME.assetModel item
    , "status"    .= T.pack (show (ME.assetStatus item))
    , "locationId" .= fmap toPathPiece (ME.assetLocationId item)
    ]

instance ToJSON (Entity InputListEntry) where
  toJSON (Entity key row) = object
    [ "id"             .= toPathPiece key
    , "channel"        .= ME.inputRowChannelNumber row
    , "trackName"      .= ME.inputRowTrackName row
    , "instrument"     .= ME.inputRowInstrument row
    , "micId"          .= fmap toPathPiece (ME.inputRowMicId row)
    , "standId"        .= fmap toPathPiece (ME.inputRowStandId row)
    , "cableId"        .= fmap toPathPiece (ME.inputRowCableId row)
    , "preampId"       .= fmap toPathPiece (ME.inputRowPreampId row)
    , "insertOutboard" .= fmap toPathPiece (ME.inputRowInsertOutboardId row)
    , "converter"      .= ME.inputRowConverterChannel row
    , "phantom"        .= ME.inputRowPhantom row
    , "polarity"       .= ME.inputRowPolarity row
    , "hpf"            .= ME.inputRowHpf row
    , "pad"            .= ME.inputRowPad row
    , "notes"          .= ME.inputRowNotes row
    ]

listInventoryDB :: SqlPersistT IO [Entity InventoryItem]
listInventoryDB = selectList [] [Asc ME.AssetName]

seedInventoryDB :: SqlPersistT IO ()
seedInventoryDB = seedInventoryAssets

seedHQDB :: UTCTime -> SqlPersistT IO ()
seedHQDB = seedHolgerSession

fetchSessionInputRowsByIndex
  :: Int
  -> SqlPersistT IO (Maybe (Entity ME.Session, [Entity InputListEntry]))
fetchSessionInputRowsByIndex idx = do
  sessions <- selectList [] [Asc ME.SessionStartAt]
  case drop (max 0 (idx - 1)) sessions of
    (sessionEnt:_) -> do
      rows <- loadLatestInputRows (entityKey sessionEnt)
      pure (Just (sessionEnt, rows))
    [] -> pure Nothing

fetchSessionInputRowsByKey
  :: ME.SessionId
  -> SqlPersistT IO (Maybe (Entity ME.Session, [Entity InputListEntry]))
fetchSessionInputRowsByKey sessionKey = do
  mSession <- getEntity sessionKey
  case mSession of
    Nothing        -> pure Nothing
    Just sessionEnt -> do
      rows <- loadLatestInputRows sessionKey
      pure (Just (sessionEnt, rows))

loadLatestInputRows
  :: ME.SessionId
  -> SqlPersistT IO [Entity InputListEntry]
loadLatestInputRows sessionKey = do
  mList <- selectFirst [ME.InputListSessionId ==. sessionKey] []
  case mList of
    Nothing -> pure []
    Just (Entity listId _) -> do
      mVersion <- selectFirst
        [ ME.InputListVersionInputListId ==. listId ]
        [ Desc ME.InputListVersionVersion
        , Desc ME.InputListVersionCreatedAt
        , LimitTo 1
        ]
      case mVersion of
        Nothing -> pure []
        Just (Entity versionId _) ->
          selectList
            [ ME.InputRowVersionId ==. versionId ]
            [ Asc ME.InputRowChannelNumber ]

renderInputListLatex :: Text -> [Entity InputListEntry] -> Text
renderInputListLatex title rows =
  let escapedTitle = latexEscape title
      bodyLines    = map renderRow rows
  in T.unlines $
       [ "\\documentclass[a4paper,landscape,10pt]{article}"
       , "\\usepackage[margin=15mm]{geometry}"
       , "\\usepackage{array,booktabs,longtable,xcolor}"
       , "\\definecolor{rowalt}{RGB}{246,246,246}"
       , "\\rowcolors{2}{rowalt}{white}"
       , "\\begin{document}"
       , "\\section*{Input List --- " <> escapedTitle <> "}"
       , "\\small"
       , "\\begin{longtable}{@{}c l l c l l c l@{}}"
       , "\\toprule"
       , "\\# & Fuente & Mic/DI & Medusa & Preamp & Interfaz & DAW & Notas \\\\"
       , "\\midrule"
       ]
       ++ bodyLines ++
       [ "\\bottomrule"
       , "\\end{longtable}"
       , "\\end{document}"
       ]
  where
    renderRow (Entity _ row) =
      let noteMap      = notesToMap (ME.inputRowNotes row)
          medusaVal    = Map.lookup "Medusa" noteMap
          preampVal    = Map.lookup "Preamp" noteMap
          interfaceVal = Map.lookup "Interface" noteMap <|> ME.inputRowConverterChannel row
          dawVal       = Map.lookup "DAW Ch" noteMap
          extraNotes   = Map.lookup "Notes" noteMap
          cells =
            [ showText (ME.inputRowChannelNumber row)
            , maybe "-" id (ME.inputRowTrackName row)
            , maybe "-" id (ME.inputRowInstrument row)
            , maybe "-" id medusaVal
            , maybe "-" id preampVal
            , maybe "-" id interfaceVal
            , maybe "-" id dawVal
            , maybe "" id extraNotes
            ]
      in T.intercalate " & " (map latexEscape cells) <> " \\\\"

    showText :: Show a => a -> Text
    showText = T.pack . show

notesToMap :: Maybe Text -> Map.Map Text Text
notesToMap Nothing = Map.empty
notesToMap (Just txt) =
  Map.fromList $ mapMaybe parseChunk (T.splitOn "|" txt)
  where
    parseChunk chunk =
      let trimmed = T.strip chunk
          (key, rest) = T.breakOn ":" trimmed
      in case T.stripPrefix ":" rest of
           Nothing    -> Nothing
           Just value -> Just (T.strip key, T.strip value)

latexEscape :: Text -> Text
latexEscape = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '&'  -> "\\&"
      '%'  -> "\\%"
      '$'  -> "\\$"
      '#'  -> "\\#"
      '_'  -> "\\_"
      '{'  -> "\\{"
      '}'  -> "\\}"
      '~'  -> "\\textasciitilde{}"
      '^'  -> "\\textasciicircum{}"
      '\\' -> "\\textbackslash{}"
      _    -> T.singleton c

generateInputListPdf :: Text -> IO (Either Text BL.ByteString)
generateInputListPdf latex = do
  let tmpDir  = "/tmp/tdf"
      texFile = tmpDir </> "inputlist.tex"
      pdfFile = tmpDir </> "inputlist.pdf"
  createDirectoryIfMissing True tmpDir
  TIO.writeFile texFile latex
  (exitCode, _out, err) <- readProcessWithExitCode "tectonic" ["-Z", "shell-escape", "-o", tmpDir, texFile] ""
  case exitCode of
    ExitSuccess -> do
      pdfBytes <- BL.readFile pdfFile
      safeRemove texFile
      safeRemove pdfFile
      pure (Right pdfBytes)
    ExitFailure code -> do
      safeRemove texFile
      let errMsg = T.concat
            [ "tectonic exited with "
            , T.pack (show code)
            , ": "
            , T.strip (T.pack err)
            ]
      pure (Left errMsg)

safeRemove :: FilePath -> IO ()
safeRemove path = removeFile path `catch` handleErr
  where
    handleErr :: IOException -> IO ()
    handleErr _ = pure ()

sanitizeFileName :: Text -> Text
sanitizeFileName txt =
  let normalised = T.map normalizeChar (T.toLower txt)
      filtered   = T.filter (\c -> isAlphaNum c || c == '-') normalised
  in if T.null filtered then "session-input-list" else T.take 64 filtered
  where
    normalizeChar c
      | c == ' '  = '-'
      | c == '_'  = '-'
      | c == '/'  = '-'
      | otherwise = c
