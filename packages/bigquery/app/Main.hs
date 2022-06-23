{-# LANGUAGE DeriveGeneric, LambdaCase, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables,
             TemplateHaskell #-}

module Main where

import           Control.Lens                    (lens, makeLenses, view, (^.))
import           Control.Monad.Google            as Google
import           Gogol                           (HasEnv (..))
import           Gogol.Compute.Metadata          (getProjectId)
import qualified Network.Google.BigQuery.Dataset as BQ
import qualified Network.Google.BigQuery.Table   as BQ
import           Network.Google.BigQuery.Types   (BigQueryScopes, Project (..))
import           Options.Applicative
import           Relude
import           Text.Printf

import qualified Gogol.Auth                      as Google
import           Gogol.Internal.Auth             (Credentials (..), _serviceId)


-- * Data types
------------------------------------------------------------------------------
data Opts
  = Opts
      { _project :: Text
      , _dataset :: Text
      , _tableId :: Text
      , _verbose :: Bool
      }
  deriving (Generic, Show)

makeLenses ''Opts

instance BQ.HasProject Opts Project where
  projectOf = lens (coerce . _project) $ \r s -> r { _project = BQ.getProject s }


-- * Helpers
------------------------------------------------------------------------------
parser :: Parser Opts
parser  = Opts
  <$> option auto (short 'p' <> long "project" <> metavar "PROJECT" <>
                   value "bigquery-public-data" <>
                   help "Google Cloud project name" <> showDefault)
  <*> option auto (short 'd' <> long "dataset" <> metavar "DATASET-ID" <>
                   value "covid19_open_data" <> help "Dataset to query run" <>
                   showDefault)
  <*> option auto (short 't' <> long "table" <> metavar "TABLE-ID" <>
                   value "covid19_open_data" <>
                   help "Table identifier for query" <> showDefault)
  <*> switch      (short 'v' <> long "verbose" <> help "Extra debug output")

------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId :: Opts -> Google BigQueryScopes Project
googleProjectId opts = do
  liftIO (lookupEnv "GCLOUD_PROJECT") >>= \case
    Just pr -> pure $ Project $ fromString pr
    Nothing -> pure $ opts ^. BQ.projectOf
{-- }
    Nothing -> do
      man <- view envManager
      Project <$> getProjectId man
--}

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
  let opts = info parser mempty
  args <- execParser opts
  print args

  withGoogle $ do
    showCreds
    proj <- googleProjectId args
    liftIO . putStrLn $ printf "Project: %s" (coerce proj :: Text)
    liftIO . mapM_ print =<< BQ.listDatasets proj
    liftIO . mapM_ print =<< BQ.listTables proj (BQ.DatasetId $ args ^. dataset)
  putTextLn "todo ..."
