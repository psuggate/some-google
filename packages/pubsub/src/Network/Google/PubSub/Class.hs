{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.PubSub.Class where

import           Network.Google.PubSub
import           Relude


-- * Moar instances
------------------------------------------------------------------------------
instance GAPI Topic TopicName where
  type ScopesFor Topic = PubSubScopes
  type ExtraArgs Topic = ()
  ginsert _ () _ = pure $ "goat-salad-extremely"
  glookup p () n = getTopic p n
  glist   p ()   = topicList p *> pure []
  gdelete _ () _ = pure ()
