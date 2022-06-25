{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, RecordWildCards, TypeApplications,
             TypeFamilies #-}

module Network.Google.BigQuery.Table
  (
    module Export

  , createTable
  , lookupTable
  , listTables

  , fromTable
  )
where

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (?~), (^.))
import           Control.Monad.Google          as Export
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth                    as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude                        hiding (set)
import           Text.Printf

import           Data.Maybe                    (fromJust)


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
    let g BQ.Table{..} = tableReference
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
      g BQ.TableReference{..} = fmap TableId tableId
      s (BQ.TableReference md mp _) =
        BQ.TableReference md mp <<< fmap getTableId

------------------------------------------------------------------------------
instance GHasRef BQ.TableList_TablesItem (Maybe BQ.TableReference) where
  gref :: Lens' BQ.TableList_TablesItem (Maybe BQ.TableReference)
  gref  =
    let g BQ.TableList_TablesItem{..} = tableReference
        s x@BQ.TableList_TablesItem{..} y =
          x { BQ.tableReference = y } :: BQ.TableList_TablesItem
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
toTable BQ.Table{..} = Table
  { table'id = fromJust $ tableReference >>= (^.guid)
  , table'name = friendlyName
  , table'schema = maybe (error "missing schema") toSchema schema
  , table'partition = prt
  , table'type = readMaybe . toString =<< type'
  }
  where
    typ = BQ.type' :: BQ.TimePartitioning -> Maybe Text
    prt = timePartitioning >>= typ >>= readMaybe . toString

------------------------------------------------------------------------------
toSchema :: BQ.TableSchema -> Schema
toSchema BQ.TableSchema{..} = Schema $ case fields of
  Nothing -> []
  Just fs -> toFields fs
  where
    typE =
      let disp = printf "invalid field 'type' value: '%s'" :: Text -> String
      in  error . fromString . disp . show
    toFields (BQ.TableFieldSchema{..}:rs) = Field
      { field'name = fromJust name
      , field'mode = readMaybe . toString =<< mode
      , field'type = fromMaybe (typE $ fromJust type') $ readMaybe . toString =<< type'
      , field'fields = toFields <$> fields
      }:toFields rs
    toFields [] = []

fromSchema :: Schema -> BQ.TableSchema
fromSchema  = BQ.TableSchema . Just . map fromFields . schema'fields
  where
    mkField :: Text -> Maybe FieldMode -> FieldType -> BQ.TableFieldSchema
    mkField n m t = BQ.newTableFieldSchema
      { BQ.name  = Just n
      , BQ.mode  = show <$> m
      , BQ.type' = Just $ show t
      }

    fromFields :: Field -> BQ.TableFieldSchema
    fromFields (Field n m t Nothing) = case t of
      STRUCT -> errT t
      RECORD -> errT t
      -- GEOGRAPHY -> err "unsupported field-type: %s" (show t)
      _      -> mkField n m t
      where
        errT = err "field of '%s' type should have subfields" . show
    fromFields (Field n m t fs) = case t of
      RECORD -> subFields n m fs
      STRUCT -> subFields n m fs
      _ -> err "field 'type' (%s) should not have subfields" (show t :: Text)

    subFields :: Text -> Maybe FieldMode -> Maybe [Field] -> BQ.TableFieldSchema
    subFields n m fs = (mkField n m RECORD) { BQ.fields = map fromFields <$> fs }

------------------------------------------------------------------------------
err :: String -> Text -> a
err msg = error . fromString . printf msg
