{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, FlexibleInstances, InstanceSigs,
             LambdaCase, MultiParamTypeClasses, NamedFieldPuns,
             NoImplicitPrelude, OverloadedStrings, RankNTypes, RecordWildCards,
             TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.BigQuery.Dataset
  (
    module Export
  , Dataset (..)

  , createDataset
  , lookupDataset
  , listDatasets
  )
where

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (^.))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude

import qualified Data.List                     as List
import           Data.Maybe                    (fromJust)


-- * Data types for BigQuery datasets
------------------------------------------------------------------------------
data Dataset
  = Dataset
      { dataset'id       :: DatasetId
      , dataset'project  :: Project
      , dataset'name     :: Maybe Text
      , dataset'location :: Maybe Text
      }
  deriving (Eq, Generic, NFData, Show)

instance FromJSON Dataset where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Dataset where toJSON = genericToJSON jsonOpts'

instance GHasId Dataset DatasetId where
  guid = lens dataset'id $ \r s -> r { dataset'id = s }


-- * Instances
------------------------------------------------------------------------------
instance GPut Dataset DatasetId where
  type PutAuth Dataset = '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ]
  type PutArgs Dataset = ()
  gput pid () dset = GoogleT $ do
    env <- ask
    let dref = toReference pid $ dset ^. guid
        dreq = BQ.newDataset
          { BQ.datasetReference = Just dref
          } :: BQ.Dataset
        proj = coerce pid
    dat <- env `Google.send` BQ.newBigQueryDatasetsInsert dreq proj
    pure $ fromJust (toDataset dat) ^. guid

-- | Retreive the metadata for the indicated dataset.
instance GGet Dataset DatasetId where
  type GetAuth Dataset = '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ]
  type GetArgs Dataset = ()
  gget (Project pid) () (DatasetId did) = GoogleT $ do
    let cmd = BQ.newBigQueryDatasetsGet did pid
    view environment >>= fmap (fromJust . toDataset) . flip Google.send cmd

-- | List the known datasets.
instance GList DatasetId where
  type ListAuth DatasetId = '[ BQ.Bigquery'FullControl
                             , BQ.CloudPlatform'FullControl
                             , BQ.CloudPlatform'ReadOnly ]
  type ListArgs DatasetId = ()
  glist (Project pid) () = GoogleT $ do
    let cmd = BQ.newBigQueryDatasetsList pid
        res :: BQ.DatasetList -> [BQ.DatasetList_DatasetsItem]
        res = fromMaybe [] . BQ.datasets
    view environment >>=
      (mapMaybe (^.guid) . res <$>) . flip Google.send cmd

{-- }
instance GAPI Dataset DatasetId where
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
--}

------------------------------------------------------------------------------
instance GHasRef BQ.DatasetList_DatasetsItem (Maybe BQ.DatasetReference) where
  gref :: Lens' BQ.DatasetList_DatasetsItem (Maybe BQ.DatasetReference)
  gref  =
    let g BQ.DatasetList_DatasetsItem{datasetReference} = datasetReference
        s x y = x { BQ.datasetReference = y } :: BQ.DatasetList_DatasetsItem
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
        s x y = x { BQ.projectId = getProject <$> y } :: BQ.DatasetReference
    in  lens g s


-- * BigQuery dataset top-level API
------------------------------------------------------------------------------
createDataset
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ] scopes
  => Project
  -> Dataset
  -> Google scopes DatasetId
createDataset prj = gput prj ()

lookupDataset
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => Project
  -> DatasetId
  -> Google scopes Dataset
lookupDataset prj = gget prj ()

listDatasets
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => Project
  -> Google scopes [DatasetId]
listDatasets prj = glist prj ()


-- * Conversion helpers
------------------------------------------------------------------------------
toDataset :: BQ.Dataset -> Maybe Dataset
toDataset BQ.Dataset{datasetReference, friendlyName, location} = Dataset
  <$> ((^.guid) =<< datasetReference)
  <*> ((^.projectOf) =<< datasetReference)
  <*> Just friendlyName
  <*> Just location

toReference :: Project -> DatasetId -> BQ.DatasetReference
toReference (Project pid) (DatasetId did) = BQ.DatasetReference
  { BQ.datasetId = Just did
  , BQ.projectId = Just pid
  }


-- * Helpers
------------------------------------------------------------------------------
jsonOpts' :: Options
jsonOpts'  = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = List.tail . List.dropWhile (/= '\'')
  }
