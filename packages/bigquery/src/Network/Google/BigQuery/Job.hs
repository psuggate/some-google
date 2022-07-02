{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GeneralisedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job
  (
    module Export

--   , insertJob
  , lookupJob
  , listJobs
--   , cancelJob
--   , deleteJob

--   , create
  , results
--   , query
  )
where

import           Control.Monad.Google              as Export
import           Data.Aeson                        as Aeson (FromJSON)
import           Data.Google.Types                 as Export
import qualified Gogol.Auth.Scope                  as Google
import qualified Gogol.BigQuery                    as BQ
import           Network.Google.BigQuery.Job.Query as Export
import           Relude


-- * API for BigQuery jobs
------------------------------------------------------------------------------
{-- }
-- | Row-insertion job.
insertJob
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ] scopes
  => Project
  -> Job
  -> Google scopes JobId
insertJob prj job = gput prj Nothing job
--}

------------------------------------------------------------------------------
-- | Lookup up status of an existing job.
lookupJob
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => Project
  -> Maybe Location
  -> JobId
  -> Google scopes Job
lookupJob  = gget

-- | List all known jobs.
listJobs
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => Project
  -> Google scopes [JobId]
listJobs prj = glist prj newJobsListArgs

------------------------------------------------------------------------------
-- | Cancel an existing job.
cancelJob
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ] scopes
  => Project
  -> Maybe Location
  -> JobId
  -> Google scopes ()
cancelJob _ _ _ = pure ()

-- | Delete an existing job.
deleteJob
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ] scopes
  => Project
  -> Maybe Location
  -> JobId
  -> Google scopes ()
deleteJob prj loc jid = pure () -- gdel prj loc gid


-- ** Query existing datasets
------------------------------------------------------------------------------
-- | Create an asynchronous data-query job.
create
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl ] scopes
  => HasEnv scopes e
  => MonadUnliftIO m
  => Job
  -> GoogleT e scopes m JobId
create _ = pure undefined

results
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => HasEnv scopes e
  => MonadUnliftIO m
  => FromJSON a
  => JobId
  -> GoogleT e scopes m [a]
results _ = pure []

------------------------------------------------------------------------------
-- | Synchronously issue a query, and cancel if the query-time exceeds the
--   indicated timeout.
query
  :: Google.KnownScopes scopes
  => Google.SatisfyScope '[ BQ.Bigquery'FullControl
                          , BQ.CloudPlatform'FullControl
                          , BQ.CloudPlatform'ReadOnly ] scopes
  => HasEnv scopes e
  => MonadUnliftIO m
  => FromJSON a
  => Job
  -> GoogleT e scopes m [a]
query _ = pure []
