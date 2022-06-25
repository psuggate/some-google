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

import           Network.Google.BigQuery.Job.Types as Export
import           Relude

{-- }
import           Control.Lens                      (Lens', lens, over, set,
                                                    view, (?~), (^.))
import           Control.Monad.Google              as Export
import           Data.Aeson                        as Aeson (FromJSON)
import           Data.Google.Types                 as Export
import qualified Gogol                             as Google
import qualified Gogol.Auth                        as Google
import qualified Gogol.Auth.Scope                  as Google
import qualified Gogol.BigQuery                    as BQ
import           Network.Google.BigQuery.Types     as Export
--}


type SQL = Text


queryJob :: Project -> DatasetId -> SQL -> Google BigQueryScopes JobId
queryJob _ _ _ = pure $ JobId "salad-potato-camera"
-- queryJob prj did sql = pure undefined
