{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Util
  (
    module Export

  , serviceName
  , truckService

  , generateMessage
  , message'
  , sentence
  , someWords

  , randIntN
  , yaml
  , indent

  , package
  , version
  )
where

import           Data.Aeson              as Aeson
import qualified Data.ByteString.Char8   as BS
import qualified Data.Char               as Char
import           Data.Event.Status       as Export
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
  message' >>= toStatusEvent plt svc

message' :: IO StatusMessage
message'  = do
  let lev = [Trace ..Fatal]
  sev <- (lev!!) <$> randIntN (length lev)
  st' <- ([resolved, running, unknown]!!) <$> randIntN 3
  StatusMessage sev st' <$> sentence

------------------------------------------------------------------------------
-- | Assemble some words into a sentence.
sentence :: IO Text
sentence  = do
  let go 0 = pure []
      go n = do
        w <- (someWords!!) <$> randIntN l
        (w:) <$> go (n-1)
      l = length someWords
  s  <- (+) <$> succ `fmap` randIntN 6 <*> randIntN 6
  ws <- go s
  let cap (w:ws') = (Char.toUpper (Text.head w) `Text.cons` Text.tail w):ws'
  pure $ Text.unwords (cap ws) <> "."

someWords :: [Text]
someWords  = ["aardvark", "sailor", "running", "accentuates", "documentation"
             ,"implements", "tagline", "secure", "Spartacus", "jovial", "by"
             ,"entropy", "truck", "graphene", "holistically", "tennis", "oaf"
             ,"butchered", "talon", "variable", "encouraging", "downright"
             ,"pterodactyl", "dazzling", "below", "internecene", "sheepishly"
             ,"complicated", "extreme", "onomatopoeia", "valid", "asphalt"
             ,"stretched", "salty", "strapping", "femur", "oxygenate", "fresh"
             ]


-- * Miscellaneous functions
------------------------------------------------------------------------------
-- | Generate a random integer within [0, n).
randIntN :: Int -> IO Int
randIntN n = floor . (*fromIntegral n) <$> (randomIO :: IO Double)

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
