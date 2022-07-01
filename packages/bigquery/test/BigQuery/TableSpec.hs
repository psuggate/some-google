{-# LANGUAGE DataKinds, NoImplicitPrelude, OverloadedStrings,
             TypeApplications #-}

module BigQuery.TableSpec where

import           Control.Monad.Google
import           Data.Google.Types
import           Relude
import           Test.Hspec

import qualified Gogol.BigQuery                as BigQuery
import           Network.Google.BigQuery.Table


spec :: Spec
spec  = describe "Pub/Sub tests" $ do

  context "Lists of topics" $ do
    let prj = "sixthsense-iot" :: Project
        did = "jasper_sims" :: DatasetId

    it "can fetch a list of topics" $ do
      res <- withGoogle (tableList prj did :: Google '[BigQuery.CloudPlatform'ReadOnly] [Table])
      -- res `shouldBe` []
      const () res `shouldBe` ()
