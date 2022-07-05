{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleContexts,
             GeneralisedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings,
             RankNTypes, RecordWildCards, ScopedTypeVariables, TupleSections,
             TypeFamilies #-}

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

import           Control.Lens                   (view)
import           Control.Monad.Google           as Export
import           Data.Aeson                     as Aeson
import qualified Gogol                          as Google
import qualified Gogol.Auth.Scope               as Google
import qualified Gogol.BigQuery                 as BQ
import           Network.Google.BigQuery.Schema
import           Network.Google.BigQuery.Types  as Export
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
  => Google.SatisfyScope (Google.Scopes BQ.BigQueryTabledataInsertAll) scopes
--   => Google.SatisfyScope '[ BQ.Bigquery'FullControl
--                           , BQ.Bigquery'Insertdata
--                           , BQ.CloudPlatform'FullControl ] scopes
  => ToJSON a
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
  :: forall scopes a. Google.KnownScopes scopes
--   => Google.SatisfyScope '[ BQ.Bigquery'FullControl
--                           , BQ.CloudPlatform'FullControl
--                           , BQ.CloudPlatform'ReadOnly ] scopes
  => Google.SatisfyScope (Google.Scopes BQ.BigQueryTabledataList) scopes
  => HasSchema a
  => Project
  -> DatasetId
  -> TableId
  -> Maybe Word32
  -> Google scopes [a]
list prj did tid num = GoogleT $ do
  let cmd = (BQ.newBigQueryTabledataList (coerce did) (coerce prj) (coerce tid))
        { BQ.maxResults = num } :: BQ.BigQueryTabledataList
  res <- view environment >>= flip Google.send cmd
  pure . fromMaybe [] $ rowsOf res


-- * Conversion helpers
------------------------------------------------------------------------------
fromTableRows
  :: forall a. ToJSON a
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

newTableRows :: Foldable f => f a -> TableRows a
newTableRows  =
  TableRows False False `flip` Nothing <<< map (Nothing,) . toList
