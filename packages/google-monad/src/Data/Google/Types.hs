{-# LANGUAGE DeriveGeneric, DerivingStrategies, FlexibleContexts,
             FunctionalDependencies, GeneralisedNewtypeDeriving,
             MultiParamTypeClasses, OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.Google.Types
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Data.Google.Types
  (
    module Export
  , HasPath (..)
  , HasProject (..)
  , Project (..)

  , Base64 (..)
  , toBase64
  )
where

import           Control.Lens         (Lens', lens, (.~), (<&>))
import           Control.Monad.Google as Export hiding (Env)
import           Data.Aeson           (FromJSON, ToJSON)
import           Gogol                (Base64 (..))
import           Relude


-- * Convenience function-families
------------------------------------------------------------------------------
class HasPath t where
  pathOf :: t -> Text

class HasProject t a | t -> a where
  projectOf :: Lens' t a


-- * Core Google Cloud Platform (GCP) data types
------------------------------------------------------------------------------
newtype Project
  = Project { getProject :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromJSON, NFData, ToJSON)

instance IsString Project where
  fromString  = Project . toText

instance ToText Project where
  toText  = getProject

instance HasPath Project where
  pathOf  = mappend "projects/" . getProject

instance HasProject Project Project where
  projectOf  = id


-- * Conversions
------------------------------------------------------------------------------
toBase64 :: Text -> Base64
toBase64  = Base64 . encodeUtf8
