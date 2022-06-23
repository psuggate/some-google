{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DuplicateRecordFields,
             FlexibleContexts, FlexibleInstances, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job
  (
    module Export

  , Job (..)
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

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (?~), (^.))
import           Control.Monad.Google
import           Data.Aeson                    as Aeson (FromJSON)
import           Data.Google.Types             as Export
import qualified Gogol.BigQuery                as BQ
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


-- * Instances
------------------------------------------------------------------------------
instance GAPI Job JobId where
  type ScopesFor Job = BigQueryScopes
  type ExtraArgs Job = TableId

  ginsert :: Project -> TableId -> Job -> Google BigQueryScopes JobId
  ginsert _ _ _ = pure undefined

  glookup :: Project -> TableId -> JobId -> Google BigQueryScopes Job
  glookup _ _ _ = pure undefined

  glist :: Project -> TableId -> Google BigQueryScopes [JobId]
  glist _ _ = pure []

  gdelete :: Project -> TableId -> JobId -> Google BigQueryScopes ()
  gdelete _ _ _ = pure ()

------------------------------------------------------------------------------
instance GHasRef BQ.JobList_JobsItem (Maybe BQ.JobReference) where
  gref :: Lens' BQ.JobList_JobsItem (Maybe BQ.JobReference)
  gref  =
    let g BQ.JobList_JobsItem{..} = jobReference
        s x@BQ.JobList_JobsItem{..} y =
          x { BQ.jobReference = y } :: BQ.JobList_JobsItem
    in  lens g s

instance GHasId BQ.JobReference (Maybe JobId) where
  guid :: Lens' BQ.JobReference (Maybe JobId)
  guid  = lens g s
    where
      g BQ.JobReference{..} = fmap JobId jobId
      s (BQ.JobReference md mp _) =
        BQ.JobReference md mp <<< fmap getJobId

instance GHasId BQ.JobList_JobsItem (Maybe JobId) where
  guid :: Lens' BQ.JobList_JobsItem (Maybe JobId)
  guid  = lens ((^.gref) >=> (^.guid)) (\r s -> r & over gref (set guid s <$>))


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
