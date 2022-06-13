{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude #-}

module Network.Google.BigQuery.Tabledata
  (
    insertAll
  , list
  )
where

import           Control.Monad.Google
import           Data.Aeson                    as Aeson (FromJSON, ToJSON)
import           Network.Google.BigQuery.Types
import           Relude


-- * Table data API
------------------------------------------------------------------------------
-- | Stream the given data into the indicated table.
insertAll
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => ToJSON a
  => DatasetId
  -> TableId
  -> [a]
  -> GoogleT e BigQueryScopes m Int
insertAll _ _ _ = pure 0

-- | List all table data.
list
  :: HasEnv BigQueryScopes e
  => MonadUnliftIO m
  => FromJSON a
  => DatasetId
  -> TableId
  -> GoogleT e BigQueryScopes m [a]
list _ _ = pure []
