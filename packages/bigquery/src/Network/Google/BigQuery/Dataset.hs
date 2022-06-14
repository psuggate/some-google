{-# LANGUAGE DuplicateRecordFields, LambdaCase, NoImplicitPrelude,
             OverloadedStrings #-}

module Network.Google.BigQuery.Dataset where

import           Control.Lens                  (lens, view, (?~))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BigQuery
import           Network.Google.BigQuery.Types as Export
import           Relude


-- * BigQuery dataset top-level API
------------------------------------------------------------------------------
createDataset
  :: Project
  -> DatasetId
  -> Google BigQueryScopes BigQuery.Dataset
createDataset pid did = GoogleT $ do
  env <- ask
  let dref = BigQuery.DatasetReference
        { BigQuery.datasetId = Just (coerce did)
        , BigQuery.projectId = Just proj
        }
      dreq = BigQuery.newDataset
        { BigQuery.datasetReference = Just dref
        } :: BigQuery.Dataset
      proj = coerce pid
  env `Google.send` BigQuery.newBigQueryDatasetsInsert dreq proj

ginsert :: Dataset -> Google BigQueryScopes (Insertions DatasetId)
ginsert _ = pure (pure undefined)

------------------------------------------------------------------------------
-- | Retreive the metadata for the indicated dataset.
glookup :: DatasetId -> Google BigQueryScopes (Maybe BigQuery.Dataset)
glookup did = GoogleT $ lookupEnv "GCLOUD_PROJECT" >>= \case
  Nothing -> pure Nothing
  Just pid -> do
    let cmd = BigQuery.newBigQueryDatasetsGet (coerce did) (toText pid)
    view environment >>= fmap Just . flip Google.send cmd

------------------------------------------------------------------------------
-- | List the known datasets.
glist :: Google BigQueryScopes [BigQuery.DatasetList_DatasetsItem]
glist  = GoogleT $ lookupEnv "GCLOUD_PROJECT" >>= \case
  Nothing -> pure []
  Just pid -> do
    let cmd = BigQuery.newBigQueryDatasetsList (toText pid)
        res :: BigQuery.DatasetList -> [BigQuery.DatasetList_DatasetsItem]
        res = fromMaybe [] . BigQuery.datasets
    view environment >>= fmap res . flip Google.send cmd

gdelete :: DatasetId -> Google BigQueryScopes ()
gdelete _ = pure ()
