{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Data.Aeson                     as Aeson
import qualified Gogol.BigQuery                 as BQ
import           Relude
import           Test.Hspec

------------------------------------------------------------------------------
-- Modules under test
import           Network.Google.BigQuery.Schema
import           System.Logger.Google           (DateTime (..),
                                                 StatusEvent (..))


-- * Test data
------------------------------------------------------------------------------
testRow :: [BQ.TableCell]
testRow  =
  [ BQ.TableCell
    { BQ.v = Just (Aeson.String "8c445890-7654-44ac-801a-30536ca3b33b") }
  , BQ.TableCell
    { BQ.v = Just (Aeson.String "1.657102119810025E9") }
  , BQ.TableCell { BQ.v = Just (Aeson.String "TESTING") }
  , BQ.TableCell { BQ.v = Just (Aeson.String "logging-truck-google") }
  , BQ.TableCell { BQ.v = Just (Aeson.String "2") }
  , BQ.TableCell { BQ.v = Just (Aeson.String "resolved") }
  , BQ.TableCell
    { BQ.v = Just
      (Aeson.String "Entropy sheepishly implements complicated jovial.")
    }
  ]


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = describe "Encoding and decoding of event-types" $ do

  context "JSON conversions for logging events" $ do

    it "can decode a @TableRow@ from BigQuery as a @StatusEvent@" $ do
      let Just evt = toObject sch testRow
          sch = schemaOf (Proxy :: Proxy StatusEvent)
          rev = Aeson.fromJSON evt :: Result StatusEvent
      (const () <$> rev) `shouldBe` Success ()


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = hspec spec
