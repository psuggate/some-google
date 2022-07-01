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


-- ** Table data-types
------------------------------------------------------------------------------
data Table
  = Table
      { table'id        :: TableId
      , table'name      :: Maybe Text
      , table'schema    :: Schema
      , table'partition :: Maybe Partitioning
      , table'type      :: Maybe TableType
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


{-- }
-- ** Schema definition data types
------------------------------------------------------------------------------
-- | Schema-definition data type, that supports just a (strict) subset of the
--   BigQuery Schema functionality.
newtype Schema
  = Schema { schema'fields :: [Field] }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

-- TODO: parse via the `gogol-bigquery` data type, and then extract the
--   desired fields?
instance FromJSON Schema where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Schema where toJSON = genericToJSON jsonOpts'

------------------------------------------------------------------------------
-- | Encoding of a subset of the BigQuery schema data type.
data Field
  = Field
      { field'name   :: Text
      , field'mode   :: Maybe FieldMode
      , field'type   :: FieldType
      , field'fields :: Maybe [Field]
      }
  deriving (Eq, Generic, NFData, Show)

{-- }
data Field
  = Column Text FieldMode FieldType
  | Struct Text FieldMode [Field]
  deriving (Eq, Generic, NFData, Show)
--}

-- TODO: parse via the `gogol-bigquery` data type, and then extract the
--   desired fields?
instance FromJSON Field where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Field where toJSON = genericToJSON jsonOpts'

------------------------------------------------------------------------------
data FieldMode
  = NULLABLE
  | REQUIRED
  | REPEATED
  deriving (Enum, Eq, Generic, NFData, Ord, Read, Show)

deriving instance FromJSON FieldMode
deriving instance ToJSON   FieldMode

instance FromHttpApiData FieldMode where
  parseUrlPiece = readEither . toString

------------------------------------------------------------------------------
-- | Name of the (SQL) data type for data that is stored within the cells of
--   the corresponding column.
data FieldType
  = INTEGER
  | FLOAT
  | INT64
  | FLOAT64
  | NUMERIC
  | BOOL
  | STRING
  | BYTES
  | DATE
  | DATETIME
  | TIME
  | TIMESTAMP
  | STRUCT
  | RECORD
  | GEOGRAPHY
  deriving (Enum, Eq, Generic, NFData, Ord, Read, Show)

deriving instance FromJSON FieldType
deriving instance ToJSON   FieldType

instance FromHttpApiData FieldType where
  parseUrlPiece = readEither . toString
--}


-- * Instances
------------------------------------------------------------------------------
instance GAPI Table TableId where
  type ScopesFor Table = BigQueryScopes
  type ExtraArgs Table = DatasetId

  ginsert :: Project -> DatasetId -> Table -> Google BigQueryScopes TableId
  ginsert prj did tab = GoogleT $ do
    let cmd = BQ.newBigQueryTablesInsert (coerce did) pay (coerce prj)
        pay = fromTable prj did tab
    view environment >>= fmap (fromJust . (^.guid)) . flip Google.send cmd

  glookup :: Project -> DatasetId -> TableId -> Google BigQueryScopes Table
  glookup prj did tid = GoogleT $ do
    let cmd = BQ.newBigQueryTablesGet (coerce did) (coerce prj) (coerce tid)
    view environment >>= fmap toTable . flip Google.send cmd

  glist :: Project -> DatasetId -> Google BigQueryScopes [TableId]
  glist prj did = GoogleT $ do
    let cmd = BQ.newBigQueryTablesList (coerce did) (coerce prj)
        res :: BQ.TableList -> [BQ.TableList_TablesItem]
        res = fromMaybe [] . BQ.tables
    view environment >>=
      (catMaybes . map (^.guid) . res <$>) . flip Google.send cmd

  gdelete :: Project -> DatasetId -> TableId -> Google BigQueryScopes ()
  gdelete _ _ _ = pure ()

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


-- * API
------------------------------------------------------------------------------
-- | Create a new table.
createTable :: Project -> DatasetId -> Table -> Google BigQueryScopes TableId
createTable  = ginsert

lookupTable :: Project -> DatasetId -> TableId -> Google BigQueryScopes Table
lookupTable  = glookup

listTables :: Project -> DatasetId -> Google BigQueryScopes [TableId]
listTables  = glist

------------------------------------------------------------------------------
tableList
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => Project
  -> DatasetId
  -> Google scopes [Table]
tableList prj did = GoogleT $ do
  let cmd = BQ.newBigQueryTablesList (coerce did) (coerce prj)
      res :: BQ.TableList -> [Table]
      res = maybe [] (map fromTablesItem) . BQ.tables
  view environment >>= \env -> res <$> Google.send env cmd


-- * Conversion helpers
------------------------------------------------------------------------------
fromTable :: Project -> DatasetId -> Table -> BQ.Table
fromTable prj did Table{..} = BQ.newTable
  { BQ.tableReference = Just ref
  , BQ.friendlyName = table'name
  , BQ.schema = Just $ fromSchema table'schema
  , BQ.timePartitioning = Just prt
  , BQ.type' = show <$> table'type
  }
  where
    tid = getTableId table'id
    ref = BQ.TableReference (Just (coerce did)) (Just (coerce prj)) (Just tid)
    prt = BQ.newTimePartitioning
      { BQ.type' = show <$> table'partition } :: BQ.TimePartitioning

toTable :: BQ.Table -> Table
toTable BQ.Table{friendlyName, schema, tableReference, timePartitioning, type'} = Table
  { table'id = fromJust $ tableReference >>= (^.guid)
  , table'name = friendlyName
  , table'schema = maybe (error "missing schema") toSchema schema
  , table'partition = prt
  , table'type = readMaybe . toString =<< type'
  }
  where
    typ = BQ.type' :: BQ.TimePartitioning -> Maybe Text
    prt = timePartitioning >>= typ >>= readMaybe . toString

fromTablesItem :: BQ.TableList_TablesItem -> Table
fromTablesItem item@BQ.TableList_TablesItem{friendlyName, timePartitioning, type'} = Table
  { table'id = fromJust $ item ^. guid
  , table'name = friendlyName
  , table'schema = Schema []
  , table'partition = prt
  , table'type = readMaybe . toString =<< type'
  }
  where
    typ = BQ.type' :: BQ.TimePartitioning -> Maybe Text
    prt = timePartitioning >>= typ >>= readMaybe . toString
