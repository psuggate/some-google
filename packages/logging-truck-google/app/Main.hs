{-# LANGUAGE DataKinds, LambdaCase, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables #-}

module Main where

import           Control.Lens            (view)
import           Control.Monad.Google    as Google
import           Data.Google.Types
import           Gogol                   (HasEnv (..))
import qualified Gogol.Auth              as Google
import           Gogol.Compute.Metadata  (getProjectId)
import           Gogol.Internal.Auth     (Credentials (..), _serviceId)
import           Relude
import           Text.Printf

import           Data.Event.Status
import           Network.Google.BigQuery (DatasetId, HasSchema (..),
                                          Schema (..), TableId)
import qualified Network.Google.BigQuery as BQ
import           System.Logger.Google


------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId :: Google '[] Project
googleProjectId  = do
  liftIO (lookupEnv "GCLOUD_PROJECT") >>= \case
    Just pr -> pure $ Project $ fromString pr
    Nothing -> do
      man <- view envManager
      Project <$> getProjectId man

------------------------------------------------------------------------------
readAuth :: Google.KnownScopes scopes => Google scopes (Google.Auth scopes)
readAuth  = view envStore >>= Google.retrieveAuthFromStore

showCreds :: Google.KnownScopes scopes => Google scopes ()
showCreds  = do
  Google.Auth creds _ <- readAuth
  case creds of
    FromMetadata sid -> putTextLn $ show sid
    FromClient _ _   -> putTextLn "FromClient"
    FromAccount acc  -> do
      let cid = _serviceId acc
      putStrLn $ printf "Service account:\n - ClientId: %s\n" (coerce cid :: Text)
    FromUser _       -> putTextLn "FromUser"
    FromTokenFile fp -> putStrLn $ printf "Token file: %s" fp

createTable
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (BQ.PutAuth BQ.Table) scopes
  => Project
  -> DatasetId
  -> TableId
  -> Google scopes BQ.TableId
createTable prj did tid = do
  let tab = BQ.newTable tid scm
      scm = schemaOf (Proxy :: Proxy StatusEvent)
  BQ.createTable prj did tab


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  withGoogle $ do
    showCreds
    liftIO . print =<< googleProjectId
  putTextLn "todo ..."
