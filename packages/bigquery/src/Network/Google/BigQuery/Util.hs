{-# LANGUAGE NoImplicitPrelude #-}

module Network.Google.BigQuery.Util where

import           Data.Aeson as Aeson
import qualified Data.List  as List
import           Relude


-- * Helpers
------------------------------------------------------------------------------
jsonOpts' :: Options
jsonOpts'  = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = List.tail . List.dropWhile (/= '\'')
  }
