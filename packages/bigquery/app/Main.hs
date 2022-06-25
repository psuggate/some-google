{-# LANGUAGE DeriveGeneric, LambdaCase, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables,
             TemplateHaskell #-}

module Main where

import           Control.Lens                      (lens, makeLenses, view,
                                                    (.~), (^.))
import           Control.Monad.Google              as Google
import           Gogol                             (HasEnv (..))
import           Gogol.Compute.Metadata            (getProjectId)
import qualified Network.Google.BigQuery.Dataset   as BQ
import qualified Network.Google.BigQuery.Job       as BQ
import qualified Network.Google.BigQuery.Table     as BQ
import qualified Network.Google.BigQuery.Tabledata as BQ
import           Network.Google.BigQuery.Types     (BigQueryScopes,
                                                    DatasetId (..),
                                                    Project (..), Table,
                                                    TableId (..))
import           Options.Applicative
import           Relude
import           Text.Printf

import qualified Gogol.Auth                        as Google
import           Gogol.Internal.Auth               (Credentials (..),
                                                    _serviceId)

import qualified Data.Aeson                        as Aeson
import qualified Data.Yaml                         as YAML
import qualified Data.Yaml.Pretty                  as YAML


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
  <$> option str (short 'p' <> long "project" <> metavar "PROJECT" <>
                  value "bigquery-public-data" <>
                  help "Google Cloud project name" <> showDefault)
  <*> option str (short 'd' <> long "dataset" <> metavar "DATASET-ID" <>
                  value "covid19_open_data" <> help "Dataset to query run" <>
                  showDefault)
  <*> option str (short 't' <> long "table" <> metavar "TABLE-ID" <>
                  value "covid19_open_data" <>
                  help "Table identifier for query" <> showDefault)
  <*> switch     (short 'v' <> long "verbose" <> help "Extra debug output")

------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId :: Opts -> Google BigQueryScopes Opts
googleProjectId opts = do
  liftIO (lookupEnv "GCLOUD_PROJECT") >>= \case
    Just pr -> pure $ opts & BQ.projectOf .~ Project (fromString pr)
    Nothing -> pure opts
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

------------------------------------------------------------------------------
-- | Fetch information for the indicated table.
dumpTable :: Opts -> Google BigQueryScopes Table
dumpTable opts = do
  let prj = opts^.BQ.projectOf
      did = DatasetId $ opts^.dataset
      tid = TableId $ opts^.tableId
  tabl <- BQ.lookupTable prj did tid
  when (opts^.verbose) $ do
    let ycfg = YAML.setConfCompare compare YAML.defConfig
    putTextLn . decodeUtf8 $ YAML.encodePretty ycfg tabl
    putTextLn . decodeUtf8 $ YAML.encodePretty ycfg $ BQ.fromTable prj did tabl
  pure tabl

disp :: Aeson.ToJSON a => a -> IO ()
disp  = putTextLn . decodeUtf8 . YAML.encodePretty ycfg
  where
    ycfg = YAML.setConfCompare compare YAML.defConfig


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  let optsParser = info parser mempty
  args <- execParser optsParser
  print args

  _ <- withGoogle $ do
    showCreds
    opts <- googleProjectId args
    let proj = opts ^. BQ.projectOf
        did  = BQ.DatasetId $ opts ^. dataset
        tid  = BQ.TableId $ opts ^. tableId
    liftIO . putStrLn $ printf "Project: %s" (coerce proj :: Text)
    liftIO . mapM_ print =<< BQ.listDatasets proj
    liftIO . mapM_ print =<< BQ.listTables proj did
    dumpTable opts
    liftIO . mapM_ print =<< BQ.listJobs proj
    liftIO . mapM_ (disp :: Aeson.Value -> IO ()) =<< BQ.list proj did tid (Just 10)
  putTextLn "todo ..."
