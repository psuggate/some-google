{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Table
  (
    module Export

  , createTable
  , lookupTable
  , listTables
  )
where

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (?~), (^.))
import           Control.Monad.Google          as Export
import           Data.Google.Types             as Export
import           GHC.TypeLits
import           Gogol                         (Env, HasEnv (..))
import qualified Gogol                         as Google
import qualified Gogol.Auth                    as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude                        hiding (set)

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
  glookup _ _ _ = pure undefined

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
  , BQ.timePartitioning = Just prt
  , BQ.type' = show <$> table'type
  }
  where
    tid = getTableId table'id
    ref = BQ.TableReference (Just (coerce did)) (Just (coerce prj)) (Just tid)
    prt = BQ.newTimePartitioning
      { BQ.type' = show <$> table'partition } :: BQ.TimePartitioning
