{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables #-}

module Main where

import           Control.Lens                    (view)
import           Control.Monad.Google            as Google
import           Gogol                           (HasEnv (..))
import           Gogol.Compute.Metadata          (getProjectId)
import qualified Network.Google.BigQuery.Dataset as BQ
import           Network.Google.BigQuery.Types   (BigQueryScopes, Project (..))
import           Relude
import           Text.Printf

import qualified Gogol.Auth                      as Google
import           Gogol.Internal.Auth             (Credentials (..), _serviceId)


------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId :: Google BigQueryScopes Project
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


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  withGoogle $ do
    showCreds
    proj <- googleProjectId
    liftIO . putStrLn $ printf "Project: %s" (coerce proj :: Text)
    liftIO . mapM_ print =<< BQ.glist proj
  putTextLn "todo ..."
