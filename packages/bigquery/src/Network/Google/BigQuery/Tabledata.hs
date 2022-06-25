{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleContexts,
             GeneralisedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings,
             RankNTypes, RecordWildCards, ScopedTypeVariables #-}

module Network.Google.BigQuery.Tabledata
  (
    module Export

  , TableRows (..)
  , TableSuffix (..)

  , insertAll
  , list
  )
where

import           Control.Lens                  (view)
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import qualified Gogol                         as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude


-- * Table data-insertion types
------------------------------------------------------------------------------
data TableRows a
  = TableRows
      { ignoreUnknown :: Bool
      , skipInvalid   :: Bool
      , rowItems      :: [(Maybe Text, a)]
      , tableSuffix   :: Maybe TableSuffix
      }
  deriving (Generic)

newtype TableSuffix
  = TableSuffix { getTableSuffix :: Text }
  deriving (Generic)
  deriving newtype (FromJSON, ToJSON)


-- * Table data API
------------------------------------------------------------------------------
-- | Stream the given data into the indicated table.
insertAll
  :: ToJSON a
  => FromJSON a
  => Project
  -> DatasetId
  -> TableId
  -> [(Maybe Text, a)]
  -> Google BigQueryScopes Int
insertAll (Project prj) (DatasetId did) (TableId tid) rows = GoogleT $ do
  let cmd = BQ.newBigQueryTabledataInsertAll did req prj tid
      req = fromTableRows $ TableRows False False rows Nothing
  view environment >>=
    (maybe 0 length . BQ.insertErrors <$>) . flip Google.send cmd

------------------------------------------------------------------------------
-- | List all table data.
list
  :: ToJSON a
  => FromJSON a
  => Project
  -> DatasetId
  -> TableId
  -> Maybe Word32
  -> Google BigQueryScopes [a]
list prj did tid num = GoogleT $ do
  let cmd = (BQ.newBigQueryTabledataList (coerce did) (coerce prj) (coerce tid))
        { BQ.maxResults = num } :: BQ.BigQueryTabledataList
      row = Aeson.decode . Aeson.encode
  res <- view environment >>=
    fmap (\BQ.TableDataList{..} -> rows) . flip Google.send cmd
  pure $ maybe [] (catMaybes . map row) res


-- * Conversion helpers
------------------------------------------------------------------------------
fromTableRows
  :: forall a. ToJSON a
  => FromJSON a
  => TableRows a
  -> BQ.TableDataInsertAllRequest
fromTableRows TableRows{..} = BQ.newTableDataInsertAllRequest
  { BQ.ignoreUnknownValues = Just ignoreUnknown
  , BQ.skipInvalidRows = Just skipInvalid
  , BQ.rows = Just $ uncurry go <$> rowItems
  , BQ.templateSuffix = coerce <$> tableSuffix
  }
  where
    go :: Maybe Text -> a -> BQ.TableDataInsertAllRequest_RowsItem
    go mid = BQ.TableDataInsertAllRequest_RowsItem mid . Just . jsonObject'
--     go mid = BQ.TableDataInsertAllRequest_RowsItem mid . jsonObject
