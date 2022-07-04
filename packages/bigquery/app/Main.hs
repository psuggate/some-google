{-# LANGUAGE DataKinds, DeriveGeneric, LambdaCase, MultiParamTypeClasses,
             NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies #-}

module Main where

import           Control.Lens            (lens, makeLenses, view, (.~), (^.))
import           Control.Monad.Google    as Google
import           Gogol.Compute.Metadata  (getProjectId)
import           Network.Google.BigQuery (DatasetId (..), Location (..),
                                          Project (..), Table, TableId (..))
import qualified Network.Google.BigQuery as BQ
import           Options.Applicative
import           Relude
import           Text.Printf

import qualified Gogol.Auth              as Google
import           Gogol.Internal.Auth     (Credentials (..), _serviceId)

import qualified Data.Aeson              as Aeson
import qualified Data.Yaml               as YAML
import qualified Data.Yaml.Pretty        as YAML

import qualified Gogol.BigQuery          as BQ (QueryResponse (..))


-- * Data types
------------------------------------------------------------------------------
data Opts
  = Opts
      { _project  :: Text
      , _dataset  :: Text
      , _tableId  :: Text
      , _location :: Text
      , _verbose  :: Bool
      }
  deriving (Generic, Show)

instance Aeson.FromJSON Opts where
  parseJSON = Aeson.withObject "Opts" $ \o -> do
    p <- o Aeson..: "project"
    d <- o Aeson..: "dataset"
    t <- o Aeson..: "tableId"
    l <- o Aeson..: "location"
    v <- o Aeson..: "verbose"
    pure $ Opts p d t l v

instance Aeson.ToJSON Opts where
  toJSON (Opts p d t l v) = Aeson.object
    [ "project"  Aeson..= p
    , "dataset"  Aeson..= d
    , "tableId"  Aeson..= l
    , "location" Aeson..= t
    , "verbose"  Aeson..= v
    ]

makeLenses ''Opts

instance BQ.HasProject Opts Project where
  projectOf = lens (coerce . _project) $ \r s -> r { _project = BQ.getProject s }

instance BQ.HasSchema Opts where
  schemaOf _ = BQ.Schema [p, d, t, l, v]
    where
      p = BQ.Leaf "project"  Nothing Nothing BQ.STRING
      d = BQ.Leaf "dataset"  Nothing Nothing BQ.STRING
      t = BQ.Leaf "tableId"  Nothing Nothing BQ.STRING
      l = BQ.Leaf "location" Nothing Nothing BQ.STRING
      v = BQ.Leaf "verbose"  Nothing Nothing BQ.BOOL


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
  <*> option str (short 'l' <> long "location" <> metavar "LOCATION" <>
                  value "australia-southeast1" <> showDefault <>
                  help "Google region and datacentre location for the dataset")
  <*> switch     (short 'v' <> long "verbose" <> help "Extra debug output")

------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId
  :: Google.KnownScopes scopes
  => Opts
  -> Google scopes Opts
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
dumpTable
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (BQ.GetAuth Table) scopes
  => Opts
  -> Google scopes Table
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

runq
  :: Google.KnownScopes scopes
  => Google.SatisfyScope (BQ.GetAuth Table) scopes
  => Opts -> Google scopes ()
runq opts = do
  let prj = opts^.BQ.projectOf
      did = DatasetId $ opts^.dataset
      tid = opts^.tableId
      loc = Location $ opts^.location
      sql = printf "SELECT SUM(size) AS total FROM `%s`" tid
  res <- BQ.queryJob prj did loc (fromString sql) False
  pure () `maybe` (mapM_ print) $ BQ.rows res


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  let optsParser = info parser mempty
  args <- execParser optsParser
  print args

  _ <- withGoogle $ do
    showCreds :: Google (BQ.PutAuth Table) ()
    opts <- googleProjectId args
    let proj = opts ^. BQ.projectOf
        did  = BQ.DatasetId $ opts ^. dataset
        tid  = BQ.TableId $ opts ^. tableId
    liftIO . putStrLn $ printf "Project: %s" (coerce proj :: Text)
    liftIO . mapM_ print =<< BQ.listDatasets proj
    liftIO . mapM_ print =<< BQ.listTables proj did
    dumpTable opts
    liftIO . mapM_ print =<< BQ.listJobs proj
--     liftIO . mapM_ (disp :: Aeson.Value -> IO ()) =<< BQ.list proj did tid (Just 10)
    liftIO . mapM_ (disp :: Opts -> IO ()) =<< BQ.list proj did tid (Just 10)
    runq opts
  putTextLn "todo ..."
