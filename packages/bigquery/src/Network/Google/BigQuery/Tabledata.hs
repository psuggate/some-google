{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleContexts,
             GeneralisedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings,
             RankNTypes, RecordWildCards, ScopedTypeVariables,
             StandaloneDeriving, TupleSections, TypeFamilies #-}

module Network.Google.BigQuery.Tabledata
  (
    module Export

  , TableRows (..)
  , TableSuffix (..)

  , insertAll
  , insertAll'
  , list

  , fromTableRows
  , newTableRows
  , tableRowsWith
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

deriving instance ToJSON a => ToJSON (TableRows a)
deriving instance FromJSON a => FromJSON (TableRows a)

newtype TableSuffix
  = TableSuffix { getTableSuffix :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

instance ToText TableSuffix where toText = getTableSuffix


-- * Table data API
------------------------------------------------------------------------------
-- | Stream the given data into the indicated table.
insertAll
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (Google.Scopes BQ.BigQueryTabledataInsertAll) scopes
  => ToJSON a
  => Project
  -> DatasetId
  -> TableId
  -> TableRows a
  -> Google scopes Int
insertAll prj did tid rows = do
  maybe 0 length . BQ.insertErrors <$> insertAll' prj did tid rows

{-- }
insertAll (Project prj) (DatasetId did) (TableId tid) rows = GoogleT $ do
  let cmd = BQ.newBigQueryTabledataInsertAll did req prj tid
      req = fromTableRows rows
  env <- view environment
  maybe 0 length . BQ.insertErrors <$> Google.send env cmd
--}

insertAll'
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (Google.Scopes BQ.BigQueryTabledataInsertAll) scopes
  => ToJSON a
  => Project
  -> DatasetId
  -> TableId
  -> TableRows a
  -> Google scopes BQ.TableDataInsertAllResponse
insertAll' (Project prj) (DatasetId did) (TableId tid) rows = GoogleT $ do
  let cmd = BQ.newBigQueryTabledataInsertAll did req prj tid
      req = fromTableRows rows
  view environment >>= flip Google.send cmd

------------------------------------------------------------------------------
-- | List all table data.
list
  :: forall scopes a. Google.KnownScopes scopes
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

------------------------------------------------------------------------------
-- | Use the given function to associate a unique identifier to each row that
--   is being inserted.
tableRowsWith :: Foldable f => (a -> Maybe Text) -> f a -> TableRows a
tableRowsWith fn =
  TableRows False False `flip` Nothing <<< map (fn &&& id) . toList

newTableRows :: Foldable f => f a -> TableRows a
newTableRows  = tableRowsWith (const Nothing)
