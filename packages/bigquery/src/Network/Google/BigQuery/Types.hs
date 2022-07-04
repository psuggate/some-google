{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric,
             DerivingStrategies, DerivingVia, GADTs,
             GeneralisedNewtypeDeriving, LambdaCase, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, StandaloneDeriving,
             TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Network.Google.BigQuery.Types
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Network.Google.BigQuery.Types
  (
    module Export

  , DatasetId (..)
  , TableId (..)

  , jsonObject'
  , jsonObject
  )
where

import           Data.Aeson          as Aeson
import qualified Data.Aeson.KeyMap   as Aeson
import           Data.Google.Types   as Export (GHasId (..), Insertions (..),
                                                PageResults (..), Project (..),
                                                insertions, pageResults)
import           Data.HashMap.Strict as HashMap
import           Data.Map.Strict     as Map
import qualified Gogol.BigQuery      as BigQuery
import           Relude
import           Web.HttpApiData     (FromHttpApiData (..))


-- * Data types for BigQuery datasets and their tables
------------------------------------------------------------------------------
newtype DatasetId
  = DatasetId { getDatasetId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)

instance IsString DatasetId where fromString = DatasetId . toText


-- ** Table data-types
------------------------------------------------------------------------------
newtype TableId
  = TableId { getTableId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)


-- * Lenses & instances
------------------------------------------------------------------------------


-- * Helpers
------------------------------------------------------------------------------
-- | Convert the give value into Gogol's bizarre @JsonObject@ value.
jsonObject' :: ToJSON a => a -> BigQuery.JsonObject
jsonObject' = fromMaybe (error "cannot JSON-serialise object") . jsonObject

-- jsonObject :: (FromJSON a, ToJSON a) => a -> Maybe BigQuery.JsonObject
-- jsonObject  = fmap BigQuery.JsonObject . Aeson.decode . Aeson.encode

jsonObject :: ToJSON a => a -> Maybe BigQuery.JsonObject
jsonObject  = toJSON >>> \case
  Object kv -> Just . BigQuery.JsonObject $ toHashMap kv
  _         -> Nothing

toHashMap :: Aeson.KeyMap a -> HashMap Text a
toHashMap  = Aeson.toMapText >>> Map.toList >>> HashMap.fromList
