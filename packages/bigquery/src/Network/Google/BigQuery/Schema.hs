{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleInstances, InstanceSigs,
             NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, RankNTypes,
             RecordWildCards, ScopedTypeVariables, StandaloneDeriving,
             TupleSections #-}

module Network.Google.BigQuery.Schema
  (
    HasSchema (..)
  , HasRows (..)

  , Schema (..)
  , Field (..)
  , FieldMode (..)
  , FieldType (..)

  , toSchema
  , fromSchema
  , keys
  , toObject
  )
where

import           Data.Aeson        as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Map.Strict   as Map
import qualified Gogol.BigQuery    as BQ
import           Relude
import           Text.Printf
import           Web.HttpApiData   (FromHttpApiData (..))


-- * Data conversion functions that use a schema definition
------------------------------------------------------------------------------
class HasSchema t where
  schemaOf  :: Proxy t -> Schema
  fromCells :: [BQ.TableCell] -> Maybe t

class HasRows t where
  rowsOf :: HasSchema a => t -> Maybe [a]


-- * Instances for decoding data returned by BigQuery
------------------------------------------------------------------------------
-- | Parse the list of @TableRow@ results and build a list of row-values, and
--   a @Nothing@ result means that this failed.
instance HasRows [BQ.TableRow] where
  rowsOf :: forall a. HasSchema a => [BQ.TableRow] -> Maybe [a]
  rowsOf rs = sequenceA $ map rowOf rs
    where
      rowOf :: HasSchema a => BQ.TableRow -> Maybe a
      rowOf (BQ.TableRow r) = case r of
        Just cs -> fromCells cs
        Nothing -> Nothing

------------------------------------------------------------------------------
instance HasRows BQ.GetQueryResultsResponse where
  -- TODO: use the returned @schema@ to check that it matches the expected
  --   schema and its data-type?
  rowsOf BQ.GetQueryResultsResponse{rows} = case rows of
    Just rs -> rowsOf rs
    Nothing -> Nothing

instance HasRows BQ.QueryResponse where
  rowsOf BQ.QueryResponse{rows} = case rows of
    Just rs -> rowsOf rs
    Nothing -> Nothing

instance HasRows BQ.TableDataList where
  rowsOf BQ.TableDataList{rows} = case rows of
    Just rs -> rowsOf rs
    Nothing -> Nothing


-- * Schema definition data types
------------------------------------------------------------------------------
-- | Schema-definition data type, that supports just a (strict) subset of the
--   BigQuery Schema functionality.
newtype Schema
  = Schema { fields :: [Field] }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, NFData, ToJSON)

------------------------------------------------------------------------------
-- | Encoding of a subset of the BigQuery schema data type.
data Field
  = Node Text (Maybe FieldMode) (Maybe Text) [Field]
  | Leaf Text (Maybe FieldMode) (Maybe Text) FieldType
  deriving (Eq, Generic, NFData, Show)

instance ToJSON Field where
  toJSON (Node n mm md fs) = Aeson.object $ catMaybes
    [ Just $ "name" Aeson..= n
    , ("mode" Aeson..=) <$> mm
    , ("description" Aeson..=) <$> md
    , Just $ "type" Aeson..= RECORD
    , Just $ "fields" Aeson..= fs
    ]
  toJSON (Leaf n mm md t) = Aeson.object $ catMaybes
    [ Just $ "name" Aeson..= n
    , ("mode" Aeson..=) <$> mm
    , ("description" Aeson..=) <$> md
    , Just $ "type" Aeson..= t
    ]

instance FromJSON Field where
  parseJSON = Aeson.withObject "Field" $ \o -> do
    n  <- o .:  "name"
    t  <- o .:  "type"
    mm <- o .:? "mode"
    md <- o .:? "description"
    mf <- o .:? "fields"
    pure $ case mf of
      Nothing -> Leaf n mm md t
      Just fs -> Node n mm md fs

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
--
--   TODO:
--    + type-parameters for @BYTES@, @STRING@, @NUMERIC@, and @BIGNUMERIC@;
--
data FieldType
  = INTEGER
  | FLOAT
  | INT64
  | FLOAT64
  | BOOL
  | NUMERIC
  | BIGNUMERIC
  | STRING
  | BYTES
  | DATE
  | DATETIME
  | TIME
  | TIMESTAMP
  | STRUCT
  | RECORD
  | GEOGRAPHY
  deriving (Eq, Generic, NFData, Read, Show)

{-- }
  | NUMERIC (Maybe Int64) (Maybe Int64)
  | BIGNUMERIC (Maybe Int64) (Maybe Int64)
  | STRING (Maybe Int64)
  | BYTES (Maybe Int64)
--}

deriving instance FromJSON FieldType
deriving instance ToJSON   FieldType

instance FromHttpApiData FieldType where
  parseUrlPiece = readEither . toString


-- * Conversion helpers
------------------------------------------------------------------------------
toSchema :: BQ.TableSchema -> Schema
toSchema (BQ.TableSchema xs) = Schema $ case xs of
  Nothing -> []
  Just fs -> toFields fs
  where
    typE t = case t of
      Just t' -> "invalid field 'type' value: '%s'" `err` show t'
      Nothing -> error "missing 'type' field of a 'TableFieldSchema'"
    toFields [] = []
    toFields (BQ.TableFieldSchema{..}:rs) =
      let f' = case fields of
                 Nothing -> Leaf nm mm md t'
                 Just fs -> Node nm mm md (toFields fs)
          nm = fromMaybe (error "missing 'name' field") name
          mm = readMaybe . toString =<< mode
          md = readMaybe . toString =<< description
          t' = fromMaybe (typE type') $ readMaybe . toString =<< type'
      in  f':toFields rs

fromSchema :: Schema -> BQ.TableSchema
fromSchema  = BQ.TableSchema . Just . map fromFields . fields
  where
    mkField :: Text -> Maybe FieldMode -> Maybe Text -> FieldType -> BQ.TableFieldSchema
    mkField n m d t = BQ.newTableFieldSchema
      { BQ.name        = Just n
      , BQ.mode        = show <$> m
      , BQ.description = d
      , BQ.type'       = Just $ show t
      }

    fromFields :: Field -> BQ.TableFieldSchema
    fromFields (Node n m d fs) = subFields n m d fs
    fromFields (Leaf n m d  t) = mkField n m d t

    subFields :: Text -> Maybe FieldMode -> Maybe Text -> [Field] -> BQ.TableFieldSchema
    subFields n m d fs =
      let f' = mkField n m d RECORD
      in  f' { BQ.fields = Just $ fromFields <$> fs }

------------------------------------------------------------------------------
-- | Get the top-level keys of a schema (and preserving the correct ordering).
keys :: Schema -> [Text]
keys (Schema fs) = go fs
  where
    go (Node k _ _ _:xs) = k:go xs
    go (Leaf k _ _ _:xs) = k:go xs
    go []                = []

------------------------------------------------------------------------------
-- | Use the given schema to attempt to build a JSON @Value@ from the row of
--   @TableCell@ values.
toObject :: Schema -> [BQ.TableCell] -> Maybe Aeson.Value
toObject sc cs
  | length cs == length kv = Just $ go kv
  | otherwise              = Nothing
  where
    kv = zipWith (\k mv -> (k,) <$> mv) (keys sc) (map BQ.v cs)
    go = Aeson.Object . Aeson.fromMapText . Map.fromList . catMaybes

{-- }
cheeseTime :: Text -> Text -> Aeson.Value -> Aeson.Value
cheeseTime t k v
  | t /= k    = v
  | otherwise = case v of
      Aeson.String ts -> maybe v Aeson.String $ dateTime ts
      _               -> v

dateTime :: Text -> Maybe Text
dateTime  = toString >>> readMaybe >>> fmap (show . utctime)

systime :: Double -> SystemTime
systime x = MkSystemTime secs nano
  where
    secs = floor x
    nano = floor $ (x - fromIntegral secs) * 1e9

utctime :: Scientific -> UTCTime
utctime  = systemToUTCTime . systime . toRealFloat
--}

------------------------------------------------------------------------------
err :: String -> Text -> a
err msg = error . fromString . printf msg
