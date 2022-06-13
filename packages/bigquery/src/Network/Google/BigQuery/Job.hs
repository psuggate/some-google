{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleContexts,
             NoImplicitPrelude #-}

module Network.Google.BigQuery.Job
  (
    Job (..)
  , JobId (..)

  , insert
  , lookup
  , list
  , cancel
  , delete
  , create
  , results
  , query
  )
where

import           Control.Monad.Google
import           Data.Aeson                    as Aeson (FromJSON)
import           Network.Google.BigQuery.Types
import           Relude


-- * Data types for BigQuery jobs
------------------------------------------------------------------------------
data Job
  = Job
  deriving (Eq, Generic, NFData, Show)

newtype JobId
  = JobId { getJobId :: Text }
  deriving (Eq, Generic, NFData, Show)


-- * API for BigQuery jobs
------------------------------------------------------------------------------
-- | Row-insertion job.
insert
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => Job
  -> GoogleT e BigQueryScopes m JobId
insert _ = pure undefined

------------------------------------------------------------------------------
-- | Lookup up status of an existing job.
lookup
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => JobId
  -> GoogleT e BigQueryScopes m (Maybe Job)
lookup _ = pure Nothing

-- | List all known jobs.
list
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => JobId
  -> GoogleT e BigQueryScopes m [Job]
list _ = pure []

------------------------------------------------------------------------------
-- | Cancel an existing job.
cancel
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => JobId
  -> GoogleT e BigQueryScopes m ()
cancel _ = pure undefined

-- | Delete an existing job.
delete
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => JobId
  -> GoogleT e BigQueryScopes m ()
delete _ = pure undefined


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
