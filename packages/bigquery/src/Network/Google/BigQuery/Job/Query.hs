{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GeneralisedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job.Query
  (
    module Network.Google.BigQuery.Job.Query
  , module Export
  )
where

import           Control.Lens                      (view)
import qualified Gogol                             as Google
import qualified Gogol.Auth.Scope                  as Google
import qualified Gogol.BigQuery                    as BQ
import           Network.Google.BigQuery.Job.Types as Export
import           Relude

{-- }
import           Control.Lens                      (Lens', lens, over, set,
                                                    view, (?~), (^.))
import           Control.Monad.Google              as Export
import           Data.Aeson                        as Aeson (FromJSON)
import           Data.Google.Types                 as Export
import           Network.Google.BigQuery.Types     as Export
--}


-- * Query data types
------------------------------------------------------------------------------
type SQL = Text


-- * API functions
------------------------------------------------------------------------------
-- | Synchronously queries the indicated dataset, and using the given SQL
--   query-string.
queryJob
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (Google.Scopes BQ.BigQueryJobsQuery) scopes
  => Project
  -> DatasetId
  -> Location
  -> SQL
  -> Bool
  -> Google scopes BQ.QueryResponse
queryJob (Project prj) (DatasetId did) (Location loc) sql dry = GoogleT $ do
  let cmd = BQ.newBigQueryJobsQuery req prj
      req = BQ.newQueryRequest
        { BQ.defaultDataset = Just ref
        , BQ.dryRun = Just dry
        , BQ.location = Just loc
        , BQ.maxResults = Just 5
        , BQ.query = Just sql
        , BQ.useLegacySql = False
        } :: BQ.QueryRequest
      ref = BQ.newDatasetReference
        { BQ.datasetId = Just did
        , BQ.projectId = Just prj
        } :: BQ.DatasetReference
  view environment >>= flip Google.send cmd
