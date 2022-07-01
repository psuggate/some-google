{-# LANGUAGE DataKinds, NoImplicitPrelude, OverloadedStrings,
             TypeApplications #-}

module Main where

import           Control.Monad.Google
import           Data.Google.Types
import           Relude
import           Test.Hspec

-- import           Gogol.Auth.Scope
import           Gogol.PubSub

import           Network.Google.PubSub.Class


spec :: Spec
spec  = describe "Pub/Sub tests" $ do

  context "Lists of topics" $ do
    let prj = "sixthsense-iot" :: Project

    it "can fetch a list of topics" $ do
      -- res <- withGoogle (topicList prj @'[Pubsub'FullControl])
      -- res <- withGoogle (topicList prj :: Google '[Pubsub'FullControl] (Maybe [Topic]))
      res <- withGoogle (topicList prj :: Google PubSubScopes (Maybe [Topic]))
      (const () <$> res) `shouldBe` Just ()

{--}
    it "can fetch a @glist'@ of topics" $ do
      -- res <- withGoogle (glist' prj () :: Google '[Pubsub'FullControl] [Topic])
      res <- withGoogle (glist' prj () :: Google PubSubScopes [Topic])
      -- res <- withGoogle (glist prj () :: Google PubSubScopes [TopicName])
      res `shouldBe` []
--}


main :: IO ()
main  = hspec spec
