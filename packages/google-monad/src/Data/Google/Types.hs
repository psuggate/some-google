{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             FlexibleContexts, FunctionalDependencies,
             GeneralisedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, StandaloneDeriving #-}

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

  , PageResults (..)
  , Insertions (..)

  , Base64 (..)
  , toBase64
  )
where

import           Control.Lens         (Lens', lens, (.~), (<&>))
import           Control.Monad.Google as Export hiding (Env)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.UUID            (UUID)
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


-- ** Miscellaneous API helper data types
------------------------------------------------------------------------------
-- | Paginated results, with a next-page identifier.
--   TODO: figure out an efficient way to get the next page of results
data PageResults t
  = PageResults
      { results   :: [t]
      , pageIndex :: Int
      , pageSize  :: Int
      , nextPage  :: Maybe UUID
      }
  deriving (Generic, NFData)

deriving instance FromJSON t => FromJSON (PageResults t)
deriving instance ToJSON   t => ToJSON (PageResults t)
-- deriving instance FromHttpApiData t => FromHttpApiData (PageResults t)

instance Functor PageResults where
  fmap f rs = rs { results = fmap f (results rs) }

instance Applicative PageResults where
  pure x = PageResults [x] 1 1 Nothing
  PageResults fs i s n <*> PageResults xs _ _ _ = PageResults (fs <*> xs) i s n

------------------------------------------------------------------------------
-- | This type exists so that PUT operations return JSON objects, as this is
--   considered best-practice.
data Insertions i
  = Insertions
      { appended    :: !Int
      , identifiers :: ![i]
      }
  deriving (Eq, Generic, NFData, Show)

deriving instance FromJSON i => FromJSON (Insertions i)
deriving instance ToJSON   i => ToJSON (Insertions i)

instance Functor Insertions where
  fmap f (Insertions n xs) = n `Insertions` fmap f xs

instance Applicative Insertions where
  pure x = Insertions 1 [x]
  Insertions n fs <*> Insertions _ xs = Insertions n (fs <*> xs)


-- * Conversions
------------------------------------------------------------------------------
toBase64 :: Text -> Base64
toBase64  = Base64 . encodeUtf8
