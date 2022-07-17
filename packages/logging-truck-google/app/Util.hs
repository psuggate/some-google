{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Util
  (
    module Export

  , serviceName
  , truckService
  , package
  , version

  , generateMessage

  , yaml
  , indent
  )
where

import           Data.Aeson              as Aeson
import qualified Data.ByteString.Char8   as BS
import qualified Data.Char               as Char
import           Data.Event.Status       as Export
import           Data.Event.Status.Util  as Export
import           Data.FileEmbed
import           Data.List               ((!!))
import qualified Data.Text               as Text
import qualified Data.Yaml.Pretty        as YAML
import           Network.Google.BigQuery as Export (DatasetId, HasSchema (..),
                                                    Schema (..), TableId)
import           Relude
import           System.Random           (randomIO)


-- * Service values
------------------------------------------------------------------------------
-- | Default name of the service (that is issuing status events).
serviceName :: ServiceName
serviceName  = truckService

truckService :: ServiceName
truckService  =
  ServiceName $ "logging-truck-google" <> " (version: " <> version <> ")"


-- * Utility functions
------------------------------------------------------------------------------
generateMessage :: IO StatusEvent
generateMessage  = do
  let svc = "logging-truck-google"
      plt = testing :: Platform
  messageR >>= toStatusEvent plt svc


-- * Miscellaneous functions
------------------------------------------------------------------------------
-- | Pretty-print a JSON value as a YAML-formatted string.
yaml :: Aeson.ToJSON a => a -> Text
yaml  = decodeUtf8 . YAML.encodePretty cfg
  where
    cfg = YAML.setConfCompare compare YAML.defConfig

-- | Indent the start of each line, and by the given number of padding-spaces.
indent :: Int -> Text -> Text
indent n = Text.unlines . map (fromString (replicate n ' ') <>) . Text.lines

------------------------------------------------------------------------------
package :: ByteString
package  = $(embedFile "logging-truck-google.cabal")

version :: Text
version  = decodeUtf8 . go . BS.lines $ package where
  go [] = "unknown"
  go (b:bs)
    | h == "version:" = BS.dropWhile (flip BS.elem " \t\r") t
    | otherwise       = go bs
    where
      (h, t) = BS.splitAt 8 b
