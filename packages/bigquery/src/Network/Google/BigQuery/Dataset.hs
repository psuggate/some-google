{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, InstanceSigs, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Dataset
  (
    module Export

  , createDataset
  , lookupDataset
  , listDatasets
  )
where

import           Control.Lens                  (Lens', _Just, lens, over, set,
                                                view, (.~), (?~), (^.), (^?))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude

import           Data.Maybe                    (fromJust)


-- * Instances
------------------------------------------------------------------------------
instance GAPI Export.Dataset DatasetId where
  type ScopesFor Dataset = BigQueryScopes
  type ExtraArgs Dataset = ()

  ginsert :: Project -> () -> Dataset -> Google BigQueryScopes DatasetId
  ginsert pid _ dset = GoogleT $ do
    env <- ask
    let dref = toReference pid $ dset ^. guid
        dreq = BQ.newDataset
          { BQ.datasetReference = Just dref
          } :: BQ.Dataset
        proj = coerce pid
    dat <- env `Google.send` BQ.newBigQueryDatasetsInsert dreq proj
    pure $ fromJust (toDataset dat) ^. guid

  ------------------------------------------------------------------------------
  -- | Retreive the metadata for the indicated dataset.
  glookup
    :: Project
    -> ()
    -> DatasetId
    -> Google BigQueryScopes Dataset
  glookup pid _ did = GoogleT $ do
    let cmd = BQ.newBigQueryDatasetsGet (coerce did) (coerce pid)
    view environment >>= fmap (fromJust . toDataset) . flip Google.send cmd

  ------------------------------------------------------------------------------
  -- | List the known datasets.
  glist :: Project -> () -> Google BigQueryScopes [DatasetId]
  glist pid _ = GoogleT $ do
    let cmd = BQ.newBigQueryDatasetsList (coerce pid)
        res :: BQ.DatasetList -> [BQ.DatasetList_DatasetsItem]
        res = fromMaybe [] . BQ.datasets
    view environment >>=
      (catMaybes . map (^.guid) . res <$>) . flip Google.send cmd

  gdelete :: Project -> () -> DatasetId -> Google BigQueryScopes ()
  gdelete _ _ _ = pure ()

------------------------------------------------------------------------------
instance GHasRef BQ.DatasetList_DatasetsItem (Maybe BQ.DatasetReference) where
  gref :: Lens' BQ.DatasetList_DatasetsItem (Maybe BQ.DatasetReference)
  gref  =
    let g BQ.DatasetList_DatasetsItem{..} = datasetReference
        s x@BQ.DatasetList_DatasetsItem{..} y =
          x { BQ.datasetReference = y } :: BQ.DatasetList_DatasetsItem
    in  lens g s

instance GHasId BQ.DatasetList_DatasetsItem (Maybe DatasetId) where
  guid :: Lens' BQ.DatasetList_DatasetsItem (Maybe DatasetId)
  guid  = lens ((^.gref) >=> (^.guid)) (\r s -> r & over gref (set guid s <$>))

------------------------------------------------------------------------------
instance GHasId BQ.DatasetReference (Maybe DatasetId) where
  guid :: Lens' BQ.DatasetReference (Maybe DatasetId)
  guid  = lens g s
    where
      g BQ.DatasetReference{..} = fmap DatasetId datasetId
      s (BQ.DatasetReference _ mp) =
        BQ.DatasetReference `flip` mp <<< fmap getDatasetId

instance HasProject BQ.DatasetReference (Maybe Project) where
  projectOf =
    let g BQ.DatasetReference{..} = fmap Project projectId
        s x@BQ.DatasetReference{..} y =
          x { BQ.projectId = getProject <$> y } :: BQ.DatasetReference
    in  lens g s


-- * BigQuery dataset top-level API
------------------------------------------------------------------------------
createDataset :: Project -> Dataset -> Google BigQueryScopes DatasetId
createDataset prj = ginsert prj ()

lookupDataset :: Project -> DatasetId -> Google BigQueryScopes Dataset
lookupDataset prj = glookup prj ()

listDatasets :: Project -> Google BigQueryScopes [DatasetId]
listDatasets prj = glist prj ()


-- * Conversion helpers
------------------------------------------------------------------------------
toDataset :: BQ.Dataset -> Maybe Dataset
toDataset BQ.Dataset{..} = Dataset
  <$> ((^.guid) =<< datasetReference)
  <*> ((^.projectOf) =<< datasetReference)
  <*> Just friendlyName
  <*> Just location
  <*> Just Nothing

toReference :: Project -> DatasetId -> BQ.DatasetReference
toReference pid did = BQ.DatasetReference
  { BQ.datasetId = Just (coerce did)
  , BQ.projectId = Just (coerce pid)
  }
