{-# LANGUAGE FlexibleContexts, LambdaCase, NoImplicitPrelude, OverloadedStrings,
             TypeApplications #-}

module System.Logger.Google
  (
    module Export

  , makeLogger
  , translateSeverity

  , stdoutLogger
  , stderrLogger
  , handleLogger
  , googleLogger
  , googleLogAction
  )
where

import           Colog.Core              (LogAction)
import qualified Colog.Core              as Colog
import           Control.Lens            (lens, set, (^.))
import           Control.Monad.IO.Unlift
import           Data.Aeson              as Aeson
import qualified Data.ByteString.Builder as B
import           Data.Event.Status       as Export hiding (stderrLogger,
                                                    stdoutLogger)
import           Data.Google.Types
import qualified Gogol                   as Google
import qualified Gogol.Internal.Logger   as Google
import           Relude
import           System.IO               (hSetBinaryMode)
import qualified System.Logger.Settings  as Logger

import           Network.Google.BigQuery


-- * Instances
------------------------------------------------------------------------------
instance HasSeverity Google.LogLevel where
  severity  = lens g (const s) where
    g Google.Trace = Logger.Trace
    g Google.Debug = Logger.Debug
    g Google.Error = Logger.Error
    g Google.Info  = Logger.Info
    s Logger.Trace = Google.Trace
    s Logger.Debug = Google.Debug
    s Logger.Warn  = Google.Error
    s Logger.Error = Google.Error
    s Logger.Fatal = Google.Error
    s Logger.Info  = Google.Info

------------------------------------------------------------------------------
-- | Define and use a BigQuery @Schema@ for a @StatusEvent@, in order to
--   deserialise the given row of @TableCell@ values.
instance HasSchema StatusEvent where
  schemaOf _ =
    let i = Leaf "id" (Just REQUIRED) Nothing STRING
        d = Leaf "datetime" (Just REQUIRED) Nothing TIMESTAMP
        p = Leaf "platform" (Just REQUIRED) Nothing STRING
        s = Leaf "service" (Just REQUIRED) Nothing STRING
        l = Leaf "severity" (Just REQUIRED) Nothing INT64
        x = Leaf "status" Nothing Nothing STRING
        m = Leaf "message" Nothing Nothing STRING
    in  Schema [i, d, p, s, l, x, m]
  fromCells cs = case Aeson.fromJSON <$> toObject sc cs of
    Just (Aeson.Success x) -> Just x
    _                      -> Nothing
    where
      sc = schemaOf (Proxy :: Proxy StatusEvent)


-- * Some standard loggers
------------------------------------------------------------------------------
-- | Log to 'stdout' with minimal formatting.
stdoutLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stdoutLogger  = handleLogger stdout

-- | Log to 'stderr' with minimal formatting.
stderrLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stderrLogger  = handleLogger stderr

------------------------------------------------------------------------------
-- | Log to the given file handle, and with minimal formatting.
handleLogger
  :: MonadIO m
  => Handle
  -> Severity
  -> m (LogAction IO StatusMessage)
handleLogger h l = do
  liftIO $ do
    hSetBinaryMode h True
    hSetBuffering h LineBuffering
  pure $ LogAction $ \x -> when (x ^. severity >= l) $ do
    B.hPutBuilder h (Google.build $ x ^. detailsOf <> "\n")


-- ** Interop with Gogol
------------------------------------------------------------------------------
-- | Convert a @LogAction m StatusMessage@ logger into a form that is usable
--   with the `gogol` packages.
googleLogger
  :: MonadUnliftIO m
  => LogAction m StatusMessage
  -> m (Google.LogLevel -> B.Builder -> IO ())
googleLogger (LogAction action) = askRunInIO >>= \run -> do
  let go :: Google.Logger
      go l b = run $ action sm
        where
          l' = l ^. severity
          tx = decodeUtf8 (B.toLazyByteString b)
          sm = StatusMessage l' "Google-specific information" tx
  pure go

googleLogAction
  :: MonadIO m
  => Google.Logger
  -> LogAction m StatusMessage
googleLogAction f = LogAction $ \(StatusMessage l _ d) -> liftIO $ do
  f (set severity l Google.Debug) (Google.build d)


makeLogger
  :: (MonadReader e m, MonadIO m, Colog.HasLog e B.Builder IO)
  => Google.LogLevel
  -> m (Google.LogLevel -> B.Builder -> IO ())
makeLogger level = do
  action <- asks Colog.getLogAction
  pure $ \l b -> when (l >= level) $ action Colog.<& b
  -- pure $ \_ b -> action Colog.<& b

translateSeverity :: Google.LogLevel -> Colog.Severity
translateSeverity  = \case
  Google.Info  -> Colog.Info
  Google.Error -> Colog.Error
  Google.Debug -> Colog.Debug
  Google.Trace -> Colog.Debug
