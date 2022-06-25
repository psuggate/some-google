{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DerivingVia, FlexibleContexts, FunctionalDependencies,
             GeneralisedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, PolyKinds, StandaloneDeriving, TypeFamilies,
             TypeFamilyDependencies #-}

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

  , GAPI (..)
  , GHasId (..)
  , GHasRef (..)

  , HasPath (..)
  , HasProject (..)
  , Project (..)

  , PageResults (..)
  , pageResults

  , Insertions (..)
  , insertions

  , Base64 (..)
  , toBase64
  )
where

import           Control.Lens         (Lens')
import           Control.Monad.Google as Export hiding (Env)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.OpenApi         (ToParamSchema, ToSchema)
import           GHC.TypeLits
import           Gogol                (Base64 (..))
import           Relude
import           Web.HttpApiData      (FromHttpApiData)


-- * Convenience function-families
------------------------------------------------------------------------------
class HasPath t where
  pathOf :: t -> Text

class HasProject t a | t -> a where
  projectOf :: Lens' t a

------------------------------------------------------------------------------
-- | General class of functions for the Google API.
class GAPI t i | t -> i, i -> t where
  type ScopesFor t :: [Symbol]
  type ExtraArgs t :: Type
  ginsert :: Project -> ExtraArgs t -> t -> Google (ScopesFor t) i
  glookup :: Project -> ExtraArgs t -> i -> Google (ScopesFor t) t
  glist   :: Project -> ExtraArgs t      -> Google (ScopesFor t) [i]
  gdelete :: Project -> ExtraArgs t -> i -> Google (ScopesFor t) ()

class GHasId t i | t -> i where
  guid :: Lens' t i

class GHasRef t i | t -> i where
  gref :: Lens' t i


-- * Core Google Cloud Platform (GCP) data types
------------------------------------------------------------------------------
newtype Project
  = Project { getProject :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

deriving via Text instance ToSchema Project
deriving via Text instance ToParamSchema Project

instance IsString Project where fromString  = Project . toText
instance ToText Project where toText  = getProject
instance HasPath Project where pathOf  = mappend "projects/" . getProject
instance HasProject Project Project where projectOf  = id


-- ** Miscellaneous API helper data types
------------------------------------------------------------------------------
-- | Paginated results, with a next-page identifier.
--
--   TODO:
--    + figure out an efficient way to get the next page of results
--    + the Google API calls return a 'pageToken', so I need to use this (and
--      cache via Redis)?
--
data PageResults t
  = PageResults
      { itemsList :: [t]
      , pageIndex :: Maybe Int
      , pageSize  :: Maybe Int
      , nextPage  :: Maybe Text
      }
  deriving (Generic, NFData)

deriving instance FromJSON t => FromJSON (PageResults t)
deriving instance ToJSON   t => ToJSON (PageResults t)

-- instance FromHttpApiData t => FromHttpApiData (PageResults t)
instance ToSchema t => ToSchema (PageResults t)

instance Functor PageResults where
  fmap f rs = rs { itemsList = f <$> itemsList rs }

instance Applicative PageResults where
  pure x = PageResults [x] Nothing Nothing Nothing
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

-- instance FromHttpApiData i => FromHttpApiData (Insertions i)
instance ToSchema i => ToSchema (Insertions i)

instance Functor Insertions where
  fmap f (Insertions n xs) = n `Insertions` fmap f xs

instance Applicative Insertions where
  pure x = Insertions 1 [x]
  Insertions n fs <*> Insertions _ xs = Insertions n (fs <*> xs)


-- * Helpers
------------------------------------------------------------------------------
insertions :: Foldable f => f i -> Insertions i
insertions js = Insertions { appended = length js, identifiers = toList js }

pageResults :: Foldable f => f t -> PageResults t
pageResults js = PageResults
  { itemsList = toList js
  , pageIndex = Nothing
  , pageSize  = Nothing
  , nextPage  = Nothing
  }


-- * Conversions
------------------------------------------------------------------------------
toBase64 :: Text -> Base64
toBase64  = Base64 . encodeUtf8
