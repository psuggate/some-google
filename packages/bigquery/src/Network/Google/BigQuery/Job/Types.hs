{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric,
             DerivingVia, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralisedNewtypeDeriving,
             InstanceSigs, MultiParamTypeClasses, NamedFieldPuns,
             NoImplicitPrelude, RankNTypes, RecordWildCards,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.BigQuery.Job.Types
  (
    module Network.Google.BigQuery.Job.Types
  , module Export
  )
where

import           Control.Lens                  (Lens', lens, over, set, view,
                                                (^.))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.BigQuery                as BQ
import           Network.Google.BigQuery.Types as Export
import           Relude


-- * Data types for BigQuery jobs
------------------------------------------------------------------------------
-- TODO:
type Job = BQ.Job

{-- }
data Job
  = Job
    { job'
  deriving (Eq, Generic, NFData, Show)

data JobConfig a where
  JobCopy :: Bool -> Maybe Int64 -> CopyConfig -> JobConfig CopyConfig
  JobLoad :: Bool -> Maybe Int64 -> LoadConfig -> JobConfig LoadConfig
  JobExtract :: Bool -> Maybe Int64 -> ExtractConfig -> JobConfig ExtractConfig
  JobQuery :: Bool -> Maybe Int64 -> QueryConfig -> JobConfig QueryConfig
--}

newtype JobId
  = JobId { getJobId :: Text }
  deriving (Eq, Generic, NFData, Show)

instance ToText JobId where toText  = getJobId
deriving via Text instance ToJSON JobId


-- * Instances
------------------------------------------------------------------------------
instance GGet Job JobId where
  type GetAuth Job = '[ BQ.Bigquery'FullControl
                      , BQ.CloudPlatform'FullControl
                      , BQ.CloudPlatform'ReadOnly ]
  type GetArgs Job = Maybe Location
  gget prj loc jid = GoogleT $ do
    let cmd = (BQ.newBigQueryJobsGet (coerce jid) (coerce prj))
          { BQ.location = coerce <$> loc } :: BQ.BigQueryJobsGet
    view environment >>= (\(env :: Env scopes) -> Google.send env cmd)

{-- }
instance GPut Job JobId where
  type PutAuth Job = '[ BQ.Bigquery'FullControl
                      , BQ.CloudPlatform'FullControl ]
  type PutArgs Job = Maybe Location
  gput _ _ _ = pure undefined
--}

------------------------------------------------------------------------------
data JobsListArgs
  = JobsListArgs
      { jobsListArgs'maxResults  :: Maybe Word32
      , jobsListArgs'parentJobId :: Maybe JobId
      }
  deriving (Eq, Generic, NFData, Show)

newJobsListArgs :: JobsListArgs
newJobsListArgs  = JobsListArgs Nothing Nothing

instance GList JobId where
  type ListAuth JobId = '[ BQ.Bigquery'FullControl
                         , BQ.CloudPlatform'FullControl
                         , BQ.CloudPlatform'ReadOnly ]
  type ListArgs JobId = JobsListArgs
  glist (Project prj) arg = GoogleT $ do
    let cmd = (BQ.newBigQueryJobsList prj)
          { BQ.parentJobId = toText <$> jobsListArgs'parentJobId arg
          , BQ.maxResults = jobsListArgs'maxResults arg
          }
        res :: BQ.JobList -> [BQ.JobReference]
        res  = maybe [] (catMaybes . map (^.gref)) . BQ.jobs
    view environment >>=
      (catMaybes . map (^.guid) . res <$>) . flip Google.send cmd

{-- }
instance GAPI Job JobId where
  type ScopesFor Job = BigQueryScopes
  type ExtraArgs Job = Maybe Location

  ginsert :: Project -> Maybe Location -> Job -> Google BigQueryScopes JobId
  ginsert _ _ _ = pure undefined

  glookup :: Project -> Maybe Location -> JobId -> Google BigQueryScopes Job
  glookup prj loc jid = GoogleT $ do
    let cmd = (BQ.newBigQueryJobsGet (coerce jid) (coerce prj))
          { BQ.location = coerce <$> loc } :: BQ.BigQueryJobsGet
    view environment >>= (\(env :: Env scopes) -> Google.send env cmd)

  glist :: Project -> Maybe Location -> Google BigQueryScopes [JobId]
  glist prj _ = GoogleT $ do
    let cmd = BQ.newBigQueryJobsList (coerce prj)
        res :: BQ.JobList -> [BQ.JobReference]
        res  = maybe [] (catMaybes . map (^.gref)) . BQ.jobs
    view environment >>=
      (catMaybes . map (^.guid) . res <$>) . flip Google.send cmd

  gdelete :: Project -> Maybe Location -> JobId -> Google BigQueryScopes ()
  gdelete _ _ _ = pure ()
--}

------------------------------------------------------------------------------
instance GHasRef BQ.JobList_JobsItem (Maybe BQ.JobReference) where
  gref :: Lens' BQ.JobList_JobsItem (Maybe BQ.JobReference)
  gref  =
    let g BQ.JobList_JobsItem{jobReference} = jobReference
        s x y = x { BQ.jobReference = y } :: BQ.JobList_JobsItem
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
