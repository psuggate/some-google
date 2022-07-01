{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.PubSub.Class
  (
    module Export
  )
where

import           Network.Google.PubSub as Export
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

instance GList Topic where
  glist' p () = topicList' p
