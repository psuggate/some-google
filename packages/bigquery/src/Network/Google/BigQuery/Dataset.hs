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

ginsert :: Project -> Dataset -> Google BigQueryScopes (Insertions DatasetId)
ginsert _ _ = pure (pure undefined)

------------------------------------------------------------------------------
-- | Retreive the metadata for the indicated dataset.
glookup
  :: Project
  -> DatasetId
  -> Google BigQueryScopes (Maybe BigQuery.Dataset)
glookup pid did = GoogleT $ do
  let cmd = BigQuery.newBigQueryDatasetsGet (coerce did) (coerce pid)
  view environment >>= fmap Just . flip Google.send cmd

------------------------------------------------------------------------------
-- | List the known datasets.
glist :: Project -> Google BigQueryScopes [BigQuery.DatasetList_DatasetsItem]
glist pid = GoogleT $ do
  let cmd = BigQuery.newBigQueryDatasetsList (coerce pid)
      res :: BigQuery.DatasetList -> [BigQuery.DatasetList_DatasetsItem]
      res = fromMaybe [] . BigQuery.datasets
  view environment >>= fmap res . flip Google.send cmd

gdelete :: Project -> DatasetId -> Google BigQueryScopes ()
gdelete _ _ = pure ()
