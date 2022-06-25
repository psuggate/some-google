{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GeneralisedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job
  (
    module Export

  , insertJob
  , lookupJob
  , listJobs
  , cancelJob
  , deleteJob

  , create
  , results
  , query
  )
where

import           Control.Lens                      (Lens', lens, over, set,
                                                    view, (?~), (^.))
import           Control.Monad.Google              as Export
import           Data.Aeson                        as Aeson (FromJSON)
import           Data.Google.Types                 as Export
import qualified Gogol                             as Google
import qualified Gogol.Auth                        as Google
import qualified Gogol.Auth.Scope                  as Google
import qualified Gogol.BigQuery                    as BQ
import           Network.Google.BigQuery.Job.Types as Export
import           Relude


-- * API for BigQuery jobs
------------------------------------------------------------------------------
-- | Row-insertion job.
insertJob :: Project -> Job -> Google BigQueryScopes JobId
insertJob prj job = ginsert prj Nothing job

------------------------------------------------------------------------------
-- | Lookup up status of an existing job.
lookupJob :: Project -> Maybe Location -> JobId -> Google BigQueryScopes Job
lookupJob  = glookup

-- | List all known jobs.
listJobs :: Project -> Google BigQueryScopes [JobId]
listJobs prj = glist prj Nothing

------------------------------------------------------------------------------
-- | Cancel an existing job.
cancelJob :: Project -> Maybe Location -> JobId -> Google BigQueryScopes ()
cancelJob _ _ _ = pure ()

-- | Delete an existing job.
deleteJob :: Project -> Maybe Location -> JobId -> Google BigQueryScopes ()
deleteJob  = gdelete


-- ** Query existing datasets
------------------------------------------------------------------------------
-- | Create an asynchronous data-query job.
create
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => Job
  -> GoogleT e BigQueryScopes m JobId
create _ = pure undefined

results
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => FromJSON a
  => JobId
  -> GoogleT e BigQueryScopes m [a]
results _ = pure []

------------------------------------------------------------------------------
-- | Synchronously issue a query, and cancel if the query-time exceeds the
--   indicated timeout.
query
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => FromJSON a
  => Job
  -> GoogleT e BigQueryScopes m [a]
query _ = pure []
