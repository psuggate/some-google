{-# LANGUAGE DataKinds, LambdaCase, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables, TupleSections, TypeFamilies #-}

module Main where

import           Control.Lens            (view)
import qualified Data.Text               as Text
import           Relude
import           Text.Printf

import           Gogol                   (HasEnv (..))
import qualified Gogol.Auth              as Google
import           Gogol.BigQuery          as BQ (Bigquery'FullControl,
                                                insertErrors)
import           Gogol.Compute.Metadata  (getProjectId)
import           Gogol.Internal.Auth     (Credentials (..), _serviceId)
import           Gogol.PubSub            as PS (Pubsub'FullControl)

import           Control.Monad.Google    as Google
import           Data.Google.Types
import qualified Network.Google.BigQuery as BQ
import           System.Logger.Google
import           Util



-- * Data-types for the app
------------------------------------------------------------------------------
type AppScopes = '[Bigquery'FullControl, Pubsub'FullControl]


-- * Google Cloud functions
------------------------------------------------------------------------------
gcloud :: Google AppScopes a -> IO a
gcloud  = withGoogle

------------------------------------------------------------------------------
-- | Try to find a suitable @Project@ name/id
googleProjectId :: Google AppScopes Project
googleProjectId  = do
  liftIO (lookupEnv "GCLOUD_PROJECT") >>= \case
    Just pr -> pure $ Project $ fromString pr
    Nothing -> do
      man <- view envManager
      Project <$> getProjectId man


-- ** BigQuery functions
------------------------------------------------------------------------------
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

fakeEvents :: MonadIO m => Int -> m (BQ.TableRows StatusEvent)
fakeEvents s = do
  let go 0 = pure []
      go n = generateMessage >>= \x -> (x:) <$> go (n-1)
      fn   = Just . toText . statusEvent'id
  BQ.tableRowsWith fn <$> liftIO (go s)


-- ** Google auth
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
  putTextLn "\n== Some sweet poetry for you ==\nstatusEvent:"
  liftIO $ do
    putTextLn . indent 2 . yaml =<< generateMessage

  putTextLn "Attempting to get Google Cloud credentials ..."
  gcloud $ do
    showCreds
    proj <- googleProjectId

    let dsid = "dev_my_little_pony"
        tabi = "dev_system_events"

    liftIO . putTextLn . ("  projectId: " <>) $ toText proj
    liftIO . putTextLn . ("  datasetId: " <>) $ toText dsid

    tabs <- BQ.listTables proj dsid
    unless (elem tabi tabs) $ do
      putStrLn $ printf "creating table '%s' ..." (toText tabi)
      tid' <- createTable proj dsid tabi
      unless (tid' == tabi) $ do
        let s = "expected: " <> toText tabi
            t = "received: " <> toText tid'
        error . fromString $ printf "table identifier problem:\n %s\n %s" s t
      putTextLn "done"
    liftIO . putTextLn . ("  tableId: " <>) $ toText tabi

    putTextLn "Inserting fake status-event data ..."
    rows <- fakeEvents . (+2) =<< liftIO (randIntN 20)
    -- mapM_ (putTextLn . ("statusEvent:\n" <>) . indent 2 . yaml)
    putTextLn . indent 2 $ yaml rows
    BQ.insertAll' proj dsid tabi rows >>= pure . BQ.insertErrors >>= \case
      Nothing -> putTextLn "SUCCESS!"
      Just er -> do
        let n = length er
        putStrLn $ printf "Number of failed row-insertions: %d" n
        mapM_ (putText . indent 2 . yaml) er
    putTextLn "done"
