{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DerivingVia, FlexibleContexts, FunctionalDependencies,
             GeneralisedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, PolyKinds, RankNTypes, StandaloneDeriving,
             TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances #-}

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
  , Concat
  , Intersect

  , GHasId (..)
  , GHasRef (..)

  , GGet (..)
  , GPut (..)
  , GList (..)
  , GDel (..)

  , HasPath (..)
  , HasProject (..)
  , Project (..)

  , HasLocation (..)
  , Location (..)

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
import           Gogol.Auth.Scope     (SatisfyScope)
import           Relude
import           Web.HttpApiData      (FromHttpApiData)


-- * Type-level operations for scopes
------------------------------------------------------------------------------
-- | Merge a list of scope-lists.
type family Concat (xs :: [[Symbol]]) where
  Concat xs = Nub (Concat' xs)

type family Concat' xs where
  Concat' (x ': xs) = x ++ Concat' xs
  Concat' '[] = '[]

------------------------------------------------------------------------------
-- | Find the intersection of the two lists of auth-scopes.
type family Intersect xs ys where
  Intersect '[] _ = '[]
  Intersect _ '[] = '[]
  Intersect (x ': xs) ys = Intersect' x (Elem x ys) xs ys

type family Intersect' x (b :: Bool) xs ys where
  Intersect' x 'True  xs ys = x ': Intersect xs ys
  Intersect' _ 'False xs ys = Intersect xs ys

------------------------------------------------------------------------------
-- | Append two lists.
type family (++) xs ys where
  (++) xs '[] = xs
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

-- | Remove duplicates from a list.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Delete x xs)

-- | Remove a specific element from a list.
type family Delete x xs where
  Delete x '[] = '[]
  Delete x (x ': ys) = Delete x ys
  Delete x (y ': ys) = y ': Delete x ys

-- | Type-level 'Data.List.elem'.
type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs


-- * Convenience function-families
------------------------------------------------------------------------------
class HasPath t where
  pathOf :: t -> Text

class HasProject t a | t -> a where
  projectOf :: Lens' t a

class HasLocation t a | t -> a where
  locationOf :: Lens' t a


-- ** Google API function-families
------------------------------------------------------------------------------
-- | General class of functions for the Google API.
class GGet t i | t -> i, i -> t where
  type GetAuth t :: [Symbol]
  type GetArgs t :: Type
  gget :: KnownScopes scopes
       => SatisfyScope (GetAuth t) scopes
       => Project -> GetArgs t -> i -> Google scopes t

class GPut t r | t -> r, r -> t where
  type PutAuth t :: [Symbol]
  type PutArgs t :: Type
  gput :: KnownScopes scopes
       => SatisfyScope (PutAuth t) scopes
       => Project -> PutArgs t -> t -> Google scopes r

class GList t where
  type ListAuth t :: [Symbol]
  type ListArgs t :: Type
  glist :: KnownScopes scopes
        => SatisfyScope (ListAuth t) scopes
        => Project -> ListArgs t -> Google scopes [t]

class GDel t where
  type DelAuth t :: [Symbol]
  type DelArgs t :: Type
  gdel :: KnownScopes scopes
       => SatisfyScope (DelAuth t) scopes
       => Project -> DelArgs t -> t -> Google scopes ()

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
newtype Location
  = Location { getLocation :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

instance IsString Location where fromString  = Location . toText
instance ToText Location where toText  = getLocation


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
