{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module BigQuery.TypesSpec where

import           Data.Aeson                     as Aeson
import qualified Gogol.BigQuery                 as BQ
import           Relude
import           Test.Hspec

import           Data.Scientific                (Scientific)
import           Data.Time.Clock                (UTCTime)
import qualified Text.Read                      as Text

-- import qualified Hedgehog.Gen   as Gen
-- import qualified Hedgehog.Range as Range
-- import           Test.Hspec.Hedgehog (Gen, PropertyT, TestT, diff, forAll,
--                                       hedgehog, test, (/==), (===))

------------------------------------------------------------------------------
-- Module under test
import           Network.Google.BigQuery.Schema
import           Network.Google.BigQuery.Types


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = describe "Encoding and decoding of event-types" $ do

  context "JSON conversions for datasets" $ do

    it "can JSON-encode a @DatasetId@" $ do
      let did = DatasetId "05558c29-e918-42ae-b66c-36551672a592"
      Aeson.decode (Aeson.encode did) `shouldBe` Just did

{-- }
    it "can JSON-decode a @UTCTime@ value" $ do
      let t = Aeson.String "1.657102119810025E9"
          n = Aeson.Number 1.657102119810025e9
          u = fromJSON n :: Aeson.Result UTCTime
          v = Text.read "2022-07-06 10:08:39.810025 UTC"
      cheeseTime "t" "t" t `shouldBe` n
      u `shouldBe` Success v
--       u `shouldBe` Text.read "2022-07-06 10:08:39.810025 UTC"
--}
