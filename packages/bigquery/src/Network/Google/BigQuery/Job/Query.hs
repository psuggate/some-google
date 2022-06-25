{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, GeneralisedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             RecordWildCards, TypeFamilies #-}

module Network.Google.BigQuery.Job.Query
  (
    module Network.Google.BigQuery.Job.Query
  , module Export
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
import           Network.Google.BigQuery.Types     as Export
import           Relude


type SQL = Text


queryJob :: Project -> DatasetId -> SQL -> Google BigQueryScopes JobId
queryJob prj did sql = pure undefined
