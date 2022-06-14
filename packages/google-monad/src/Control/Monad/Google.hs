{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             GeneralisedNewtypeDeriving, InstanceSigs, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, PatternSynonyms, RankNTypes,
             ScopedTypeVariables, TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Google
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Control.Monad.Google
  (
    Google
  , GoogleT (..)

  , HasGoogle (..)
  , HasEnv (..)
  , Env
  , MonadUnliftIO

  , makeEnv
  , newGoogleEnv

  , runGoogle
  , runGoogleT
  , withGoogle
  )
where

import           Control.Lens                 (lens, view, (.~), (<&>))
import           Control.Monad.IO.Unlift      (MonadIO, MonadUnliftIO, liftIO)
import           Control.Monad.Reader         (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Proxy
import           GHC.TypeLits                 (Symbol)
import           Gogol                        (Env, HasEnv (..))
import qualified Gogol                        as Google
import qualified Gogol.Auth                   as Google
import           Relude
import           System.IO                    (stdout)


-- * Function-families for the evaluation of Google actions
------------------------------------------------------------------------------
-- | Evaluate a Google action within the current monad/context.
class HasGoogle (scopes :: [Symbol]) m where
  google :: Google scopes a -> m a


-- * Google monad
------------------------------------------------------------------------------
type Google scopes = GoogleT (Env scopes) scopes IO

{-- }
newtype Google scopes a
  = Google { getGoogle :: ReaderT (Env scopes) (ResourceT IO) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (Env scopes)
    , MonadUnliftIO
    )
--}

------------------------------------------------------------------------------
-- | Generalise the above monad to work with more environment-types, and over
--   more base-monads.
newtype GoogleT env (scopes :: [Symbol]) m a
  = GoogleT { getGoogleT :: ReaderT env (ResourceT m) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader env
    , MonadUnliftIO
    )


-- * Google instances
------------------------------------------------------------------------------
-- | Run the Google Cloud action within an appropriately-scoped context.
instance Google.KnownScopes scopes => HasGoogle scopes IO where
  google :: Google scopes a -> IO a
  google action = do
    lgr <- Google.newLogger Google.Debug stdout
    env <- Google.newEnv
      <&> (Google.envLogger .~ lgr)
      . (Google.envScopes .~ (Proxy :: Proxy scopes))
    runGoogle env action

instance Google.KnownScopes scopes => HasGoogle scopes (Google scopes) where
  google :: Google scopes a -> Google scopes a
  google action = ask >>= liftIO . flip runGoogle action

instance (MonadIO m, Google.KnownScopes scopes) =>
         HasGoogle scopes (GoogleT (Env scopes) scopes m) where
  google :: Google scopes a -> GoogleT (Env scopes) scopes m a
  google action = view environment >>= liftIO . flip runGoogle action


-- * Google environments
------------------------------------------------------------------------------
makeEnv
  :: forall m scopes. MonadIO m
  => Google.KnownScopes scopes
  => Google.LogLevel
  -> m (Env scopes)
makeEnv level = liftIO $ do
  logger <- Google.newLogger level stdout
  Google.newEnv
    <&> Google.envLogger .~ logger
    <&> Google.envScopes .~ (Proxy :: Proxy scopes)

newGoogleEnv
  :: forall m scopes. MonadIO m
  => Google.KnownScopes scopes
  => Google.Logger
  -> m (Env scopes)
newGoogleEnv logger = liftIO $ do
  Google.newEnv
    <&> Google.envLogger .~ logger
    <&> Google.envScopes .~ (Proxy :: Proxy scopes)


-- * Google evaluation
------------------------------------------------------------------------------
-- | Evaluate the @GoogleT@-based action within the given context.
runGoogleT
  :: forall env scopes m a. MonadUnliftIO m
  => Google.KnownScopes scopes
  => Google.HasEnv scopes env
  => env
  -> GoogleT env scopes m a
  -> m a
runGoogleT env = runResourceT . flip runReaderT env . getGoogleT

------------------------------------------------------------------------------
-- | Evaluate the @Google@ action within the given context.
runGoogle
  :: forall scopes a. Google.KnownScopes scopes
  => Env scopes
  -> Google scopes a
  -> IO a
runGoogle env = runResourceT . flip runReaderT env . getGoogleT

------------------------------------------------------------------------------
-- | Run the Google Cloud action within an appropriately-scoped context.
withGoogle
  :: forall scopes a. Google.KnownScopes scopes
  => Google scopes a
  -> IO a
withGoogle  = google
{-# INLINE withGoogle #-}

{-- }
------------------------------------------------------------------------------
-- | Run the Google Cloud action within an appropriately-scoped context.
withGoogle
  :: forall scopes a. Google.KnownScopes scopes
  => Google scopes a
  -> IO a
withGoogle action = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv
    <&> (Google.envLogger .~ lgr)
      . (Google.envScopes .~ (Proxy :: Proxy scopes))
  runGoogle env action
--}
