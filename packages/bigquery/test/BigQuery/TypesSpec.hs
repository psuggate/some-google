{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module BigQuery.TypesSpec where

import           Data.Aeson                    as Aeson
import           Test.Hspec

-- import           Relude
-- import qualified Hedgehog.Gen   as Gen
-- import qualified Hedgehog.Range as Range
-- import           Test.Hspec.Hedgehog (Gen, PropertyT, TestT, diff, forAll,
--                                       hedgehog, test, (/==), (===))

------------------------------------------------------------------------------
-- Module under test
import           Network.Google.BigQuery.Types


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = describe "Encoding and decoding of event-types" $ do

  context "JSON conversions for datasets" $ do

    it "can JSON-encode a @DatasetId@" $ do
      let did = DatasetId "05558c29-e918-42ae-b66c-36551672a592"
      Aeson.decode (Aeson.encode did) `shouldBe` Just did
