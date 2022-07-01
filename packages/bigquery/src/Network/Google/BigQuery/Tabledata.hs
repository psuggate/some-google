{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleContexts,
             GeneralisedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings,
             RankNTypes, RecordWildCards, ScopedTypeVariables,
             TupleSections #-}

module Network.Google.BigQuery.Tabledata
  (
    module Export

  , TableRows (..)
  , TableSuffix (..)

  , insertAll
  , list

  , fromTableRows
  , newTableRows
  )
where

import           Control.Lens                  (view)
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import qualified Gogol                         as Google
import qualified Gogol.Auth.Scope              as Google
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
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.Bigquery'Insertdata
                          , BQ.CloudPlatform'FullControl ] scopes
  => ToJSON a
  => FromJSON a
  => Project
  -> DatasetId
  -> TableId
  -> TableRows a
  -> Google scopes Int
insertAll (Project prj) (DatasetId did) (TableId tid) rows = GoogleT $ do
  let cmd = BQ.newBigQueryTabledataInsertAll did req prj tid
      req = fromTableRows rows
  view environment >>=
    (maybe 0 length . BQ.insertErrors <$>) . flip Google.send cmd

------------------------------------------------------------------------------
-- | List all table data.
list
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => ToJSON a
  => FromJSON a
  => Project
  -> DatasetId
  -> TableId
  -> Maybe Word32
  -> Google scopes [a]
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

newTableRows :: (ToJSON a, FromJSON a, Foldable f) => f a -> TableRows a
newTableRows  =
  TableRows False False `flip` Nothing <<< map (Nothing,) . toList
