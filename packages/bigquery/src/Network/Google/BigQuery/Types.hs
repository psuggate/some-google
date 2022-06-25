{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric,
             DerivingStrategies, DerivingVia, GADTs,
             GeneralisedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, StandaloneDeriving,
             TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Network.Google.BigQuery.Types
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Network.Google.BigQuery.Types
  (
    module Export

  , AllowBigQueryRequest
  , BigQueryScopes

  , DatasetId (..)
  , Dataset (..)

  , TableId (..)
  , Table (..)

  , Schema (..)
  , Field (..)
  , FieldMode (..)
  , FieldType (..)

  , jsonObject'
  , jsonObject
  )
where

import           Control.Lens      (lens)
import           Data.Aeson        as Aeson
import           Data.Google.Types as Export (GHasId (..), Insertions (..),
                                              PageResults (..), Project (..),
                                              insertions, pageResults)
import qualified Data.List         as List
import qualified Gogol.Auth.Scope  as Google
import qualified Gogol.BigQuery    as BigQuery
import           Relude
import           Web.HttpApiData   (FromHttpApiData (..))


-- * Type constraints
------------------------------------------------------------------------------
-- | Allow a Pub/Sub action to be performed if any of the required scopes are
--   present.
type AllowBigQueryRequest scopes =
  ( Google.KnownScopes scopes
  , Google.SatisfyScope BigQueryScopes scopes
  )

type BigQueryScopes
  = '[ BigQuery.CloudPlatform'FullControl
     , BigQuery.Bigquery'FullControl
     ]


-- * Data types for BigQuery datasets and their tables
------------------------------------------------------------------------------
newtype DatasetId
  = DatasetId { getDatasetId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)

instance IsString DatasetId where fromString = DatasetId . toText

data Dataset
  = Dataset
      { dataset'id       :: DatasetId
      , dataset'project  :: Project
      , dataset'name     :: Maybe Text
      , dataset'location :: Maybe Text
      , dataset'tables   :: Maybe [Table]
      }
  deriving (Eq, Generic, NFData, Show)

instance FromJSON Dataset where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Dataset where toJSON = genericToJSON jsonOpts'

instance GHasId Dataset DatasetId where
  guid = lens dataset'id $ \r s -> r { dataset'id = s }


-- ** Table data-types
------------------------------------------------------------------------------
newtype TableId
  = TableId { getTableId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)

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


-- * Lenses & instances
------------------------------------------------------------------------------


-- * Helpers
------------------------------------------------------------------------------
jsonOpts' :: Options
jsonOpts'  = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = List.tail . List.dropWhile (/= '\'')
  }

------------------------------------------------------------------------------
-- | Convert the give value into Gogol's bizarre @JsonObject@ value.
jsonObject' :: (FromJSON a, ToJSON a) => a -> BigQuery.JsonObject
jsonObject' = fromMaybe (error "cannot JSON-serialise object") . jsonObject

jsonObject :: (FromJSON a, ToJSON a) => a -> Maybe BigQuery.JsonObject
jsonObject  = fmap BigQuery.JsonObject . Aeson.decode . Aeson.encode
