{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts,
             GeneralisedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings,
             PatternSynonyms, RankNTypes, ScopedTypeVariables, TypeFamilies #-}

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
    Google (..)
  , GoogleT (..)

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

import           Control.Lens                 (lens, (.~), (<&>))
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


-- * Google monad
------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- | Generalise the above monad to work with more environment-types, and over
--   more base-monads.
newtype GoogleT env (scopes :: [Symbol]) m a
  = GoogleT { getGoogleT :: ReaderT env (ResourceT m) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader env)


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
runGoogle env = runResourceT . flip runReaderT env . getGoogle

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
