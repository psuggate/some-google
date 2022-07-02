{-# LANGUAGE ConstraintKinds, DataKinds, DuplicateRecordFields,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             NamedFieldPuns, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.PubSub.Class
  (
    module Export
  )
where

import qualified Gogol.PubSub          as PubSub
import           Network.Google.PubSub as Export
import           Relude


-- * Moar instances
------------------------------------------------------------------------------
instance GList TopicName where
  type ListAuth TopicName = '[ PubSub.CloudPlatform'FullControl
                             , PubSub.Pubsub'FullControl ]
  type ListArgs TopicName = ()
  glist prj () = mapMaybe fun <$> Export.topicList' prj
    where
      fun :: PubSub.Topic -> Maybe TopicName
      fun PubSub.Topic{name} = TopicName <$> name

{-- }
instance GAPI Topic TopicName where
  type ScopesFor Topic = PubSubScopes
  type ExtraArgs Topic = ()
  ginsert _ () _ = pure $ "goat-salad-extremely"
  glookup p () n = getTopic p n
  glist   p ()   = topicList p *> pure []
  gdelete _ () _ = pure ()

instance GList Topic where
  glist' p () = topicList' p
--}
