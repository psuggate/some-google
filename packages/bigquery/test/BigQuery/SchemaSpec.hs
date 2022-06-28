{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module BigQuery.SchemaSpec where

import           Data.Aeson                     as Aeson
import           Relude
import           Test.Hspec

------------------------------------------------------------------------------
-- Module under test
import           Network.Google.BigQuery.Schema


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = describe "Encoding and decoding of BigQuery schema-types" $ do

  context "JSON conversions for schema data types" $ do

    it "can JSON-encode a 'mode' value for a schema field" $ do
      let moe = NULLABLE
          str = show moe :: Text
      Aeson.encode moe `shouldBe` encodeUtf8 (show str :: Text)
      Aeson.decode (Aeson.encode moe) `shouldBe` Just moe

    it "can JSON-encode a @Schema@" $ do
      let sch = Schema fes
          fes = [ Leaf "id" Nothing Nothing INTEGER
                , Leaf "flavour" (Just NULLABLE) (Just "Umami?") STRING
                ]
          bqs = fromSchema sch
      Aeson.decode (Aeson.encode sch) `shouldBe` Just sch
      Aeson.decode (Aeson.encode bqs) `shouldBe` Just sch

  context "Decoding of BigQuery row-data" $ do

    it "can generate a schema from a data type" $ do
      '0' `shouldBe` '0'
