{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GeneralisedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job
  (
    module Export

  , BQ.Job (..)
  , JobId (..)

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

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (?~), (^.))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson (FromJSON)
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth                    as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude


-- * Data types for BigQuery jobs
------------------------------------------------------------------------------
type Job = BQ.Job

{-- }
data Job
  = Job
    { job'
  deriving (Eq, Generic, NFData, Show)
--}

newtype JobId
  = JobId { getJobId :: Text }
  deriving (Eq, Generic, NFData, Show)

newtype Location
  = Location { getLocation :: Text }
  deriving (Eq, Generic, NFData, Show)


-- * Instances
------------------------------------------------------------------------------
instance GAPI Job JobId where
  type ScopesFor Job = BigQueryScopes
  type ExtraArgs Job = Maybe Location

  ginsert :: Project -> Maybe Location -> Job -> Google BigQueryScopes JobId
  ginsert _ _ _ = pure undefined

  glookup :: Project -> Maybe Location -> JobId -> Google BigQueryScopes Job
  glookup prj loc jid = GoogleT $ do
    let cmd = (BQ.newBigQueryJobsGet (coerce jid) (coerce prj))
          { BQ.location = coerce <$> loc } :: BQ.BigQueryJobsGet
    view environment >>= flip Google.send cmd

  glist :: Project -> Maybe Location -> Google BigQueryScopes [JobId]
  glist prj _ = GoogleT $ do
    let cmd = BQ.newBigQueryJobsList (coerce prj)
        res :: BQ.JobList -> [BQ.JobReference]
        res  = maybe [] (catMaybes . map (^.gref)) . BQ.jobs
    view environment >>=
      (catMaybes . map (^.guid) . res <$>) . flip Google.send cmd

  gdelete :: Project -> Maybe Location -> JobId -> Google BigQueryScopes ()
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
      s x y = x { BQ.jobId = fmap getJobId y } :: BQ.JobReference

instance GHasId BQ.JobList_JobsItem (Maybe JobId) where
  guid :: Lens' BQ.JobList_JobsItem (Maybe JobId)
  guid  = lens ((^.gref) >=> (^.guid)) (\r s -> r & over gref (set guid s <$>))


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
