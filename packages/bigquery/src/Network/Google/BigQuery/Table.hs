{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleInstances, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.BigQuery.Table
  (
    module Export
  , Table (..)
  , newTable

  , Schema (..)
  , Field (..)
  , FieldMode (..)
  , FieldType (..)

  , createTable
  , lookupTable
  , listTables

  , tableList

  , fromTable
  , toTable
  , fromTablesItem
  )
where

import           Control.Lens                   (Lens', lens, over, set, view,
                                                 (^.))
import           Control.Monad.Google           as Export
import           Data.Aeson                     as Aeson
import           Data.Google.Types              as Export
import qualified Gogol                          as Google
import qualified Gogol.Auth.Scope               as Google
import qualified Gogol.BigQuery                 as BQ
import           Network.Google.BigQuery.Schema as Export
import           Network.Google.BigQuery.Types  as Export
import           Relude
import           Web.HttpApiData                (FromHttpApiData (..))

import           Data.Maybe                     (fromJust)
import           Network.Google.BigQuery.Util   (jsonOpts')


-- * Table data-types
------------------------------------------------------------------------------
data Table
  = Table
      { table'id          :: TableId
      , table'name        :: Maybe Text
      , table'description :: Maybe Text
      , table'schema      :: Maybe Schema
      , table'partition   :: Maybe Partitioning
      , table'type        :: Maybe TableType
      }
  deriving (Eq, Generic, NFData, Show)

instance FromJSON Table where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Table where toJSON = genericToJSON jsonOpts'

instance GHasId Table TableId where
  guid = lens table'id $ \r s -> r { table'id = s }

------------------------------------------------------------------------------
data Partitioning
  = DAY
  | HOUR
  | MONTH
  | YEAR
  deriving (Eq, Generic, NFData, Read, Show)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData Partitioning where
  parseUrlPiece "DAY"   = Right DAY
  parseUrlPiece "HOUR"  = Right HOUR
  parseUrlPiece "MONTH" = Right MONTH
  parseUrlPiece "YEAR"  = Right YEAR
  parseUrlPiece _       = Left "Invalid partitioning time-period"

------------------------------------------------------------------------------
data TableType
  = TABLE
  | VIEW
  | SNAPSHOT
  | MATERIALIZED_VIEW
  | EXTERNAL
  deriving (Eq, Generic, NFData, Read, Show)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData TableType where
  parseUrlPiece = readEither . toString


-- * Instances
------------------------------------------------------------------------------
instance GGet Table TableId where
  type GetAuth Table = '[ BQ.Bigquery'FullControl
                        , BQ.CloudPlatform'FullControl
                        , BQ.CloudPlatform'ReadOnly ]
  type GetArgs Table = DatasetId
  gget (Project pid) (DatasetId did) (TableId tid) = GoogleT $ do
    let cmd = BQ.newBigQueryTablesGet did pid tid
    view environment >>= fmap toTable . flip Google.send cmd

------------------------------------------------------------------------------
-- | Create a table, and using the given table-arguments.
instance GPut Table TableId where
  type PutAuth Table = '[ BQ.Bigquery'FullControl
                        , BQ.CloudPlatform'FullControl ]
  type PutArgs Table = DatasetId
  gput prj did tab = GoogleT $ do
    let cmd = BQ.newBigQueryTablesInsert (coerce did) pay (coerce prj)
        pay = fromTable prj did tab
    view environment >>= fmap (fromJust . (^.guid)) . flip Google.send cmd

------------------------------------------------------------------------------
instance GList TableId where
  type ListAuth TableId = '[ BQ.Bigquery'FullControl
                           , BQ.CloudPlatform'FullControl
                           , BQ.CloudPlatform'ReadOnly ]
  type ListArgs TableId = DatasetId
  glist (Project pid) (DatasetId did) = GoogleT $ do
    let cmd = BQ.newBigQueryTablesList did pid
        res :: BQ.TableList -> [BQ.TableList_TablesItem]
        res = fromMaybe [] . BQ.tables
    view environment >>= (mapMaybe (^.guid) . res <$>) . flip Google.send cmd

------------------------------------------------------------------------------
instance GHasRef BQ.Table (Maybe BQ.TableReference) where
  gref :: Lens' BQ.Table (Maybe BQ.TableReference)
  gref  =
    let g BQ.Table{tableReference} = tableReference
        s x y = x { BQ.tableReference = y } :: BQ.Table
    in  lens g s

instance GHasId BQ.Table (Maybe TableId) where
  guid :: Lens' BQ.Table (Maybe TableId)
  guid  = lens ((^.gref) >=> (^.guid)) (\r s -> r & over gref (set guid s <$>))

------------------------------------------------------------------------------
instance GHasId BQ.TableReference (Maybe TableId) where
  guid :: Lens' BQ.TableReference (Maybe TableId)
  guid  = lens g s
    where
      g BQ.TableReference{tableId} = fmap TableId tableId
      s (BQ.TableReference md mp _) =
        BQ.TableReference md mp <<< fmap getTableId

------------------------------------------------------------------------------
instance GHasRef BQ.TableList_TablesItem (Maybe BQ.TableReference) where
  gref :: Lens' BQ.TableList_TablesItem (Maybe BQ.TableReference)
  gref  =
    let g BQ.TableList_TablesItem{tableReference} = tableReference
        s x y = x { BQ.tableReference = y } :: BQ.TableList_TablesItem
    in  lens g s

instance GHasId BQ.TableList_TablesItem (Maybe TableId) where
  guid :: Lens' BQ.TableList_TablesItem (Maybe TableId)
  guid  = lens ((^.gref) >=> (^.guid)) (\r s -> r & over gref (set guid s <$>))


-- * Smart constructors
------------------------------------------------------------------------------
newTable :: TableId -> Schema -> Table
newTable t s = Table t Nothing Nothing (Just s) Nothing Nothing


-- * API
------------------------------------------------------------------------------
-- | Create a new table.
createTable
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (PutAuth Table) scopes
  => Project
  -> DatasetId
  -> Table
  -> Google scopes TableId
createTable  = gput

lookupTable
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (GetAuth Table) scopes
  => Project
  -> DatasetId
  -> TableId
  -> Google scopes Table
lookupTable  = gget

listTables
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (ListAuth TableId) scopes
  => Project
  -> DatasetId
  -> Google scopes [TableId]
listTables  = glist

------------------------------------------------------------------------------
tableList
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (ListAuth TableId) scopes
  => Project
  -> DatasetId
  -> Google scopes [Table]
tableList (Project pid) (DatasetId did) = GoogleT $ do
  let cmd = BQ.newBigQueryTablesList did pid
      res :: BQ.TableList -> [Table]
      res = maybe [] (map fromTablesItem) . BQ.tables
  view environment >>= \env -> res <$> Google.send env cmd


-- * Conversion helpers
------------------------------------------------------------------------------
fromTable :: Project -> DatasetId -> Table -> BQ.Table
fromTable prj did Table{..} = BQ.newTable
  { BQ.tableReference = ref table'id
  , BQ.friendlyName = table'name
  , BQ.description = table'description
  , BQ.schema = fromSchema <$> table'schema
  , BQ.timePartitioning = Just prt
  , BQ.type' = show <$> table'type
  }
  where
    ref = Just . tableRef prj did
    prt = BQ.newTimePartitioning
      { BQ.type' = show <$> table'partition } :: BQ.TimePartitioning

toTable :: BQ.Table -> Table
toTable BQ.Table{description, friendlyName, schema, tableReference, timePartitioning, type'} = Table
  { table'id          = fromJust $ tableReference >>= (^.guid)
  , table'name        = friendlyName
  , table'description = description
  , table'schema      = toSchema <$> schema
  , table'partition   = prt
  , table'type        = readMaybe . toString =<< type'
  }
  where
    typ = BQ.type' :: BQ.TimePartitioning -> Maybe Text
    prt = timePartitioning >>= typ >>= readMaybe . toString

tableRef :: Project -> DatasetId -> TableId -> BQ.TableReference
tableRef (Project pid) (DatasetId did) (TableId tid) =
  BQ.TableReference (Just did) (Just pid) (Just tid)

------------------------------------------------------------------------------
fromTablesItem :: BQ.TableList_TablesItem -> Table
fromTablesItem item@BQ.TableList_TablesItem{friendlyName, timePartitioning, type'} = Table
  { table'id = fromJust $ item ^. guid
  , table'name = friendlyName
  , table'description = Nothing
  , table'schema = Nothing
  , table'partition = prt
  , table'type = readMaybe . toString =<< type'
  }
  where
    typ = BQ.type' :: BQ.TimePartitioning -> Maybe Text
    prt = timePartitioning >>= typ >>= readMaybe . toString
