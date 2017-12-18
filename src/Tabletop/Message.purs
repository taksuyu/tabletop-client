module Tabletop.Message where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either as E
import Data.List as L

data TabletopMessage a s b
  = SystemMessage a
  | ServiceMessage String s b

instance encodeTabletopMessage :: (EncodeJson a, EncodeJson s, EncodeJson b)
                                  => EncodeJson (TabletopMessage a s b) where
  encodeJson = case _ of
    SystemMessage a ->
      "tag" := "SystemMessage"
      ~> "contents" := encodeJson a
      ~> jsonEmptyObject
    ServiceMessage uuid service b ->
      "tag" := "ServiceMessage"
      ~> "contents" := encodeJson [ encodeJson uuid, encodeJson service, encodeJson b ]
      ~> jsonEmptyObject

data SystemMessage
  = CreateGame

instance encodeSystemMessage :: EncodeJson SystemMessage where
  encodeJson = case _ of
    CreateGame ->
      fromString "CreateGame"

data TabletopResponse a b
  = SystemResponse a
  | ServiceResponse b

instance decodeTabletopResponse :: (DecodeJson a, DecodeJson b)
                                   => DecodeJson (TabletopResponse a b) where
  decodeJson j = do
    obj <- decodeJson j
    tag <- obj .? "tag"
    contents <- obj .? "contents"
    case tag of
      "SystemResponse" ->
        pure SystemResponse <*> decodeJson contents
      "ServiceResponse" ->
        pure ServiceResponse <*> decodeJson contents
      str -> E.Left $ "Expected TabletopResponse: " <> str

newtype ChannelInfo
  = ChannelInfo
    { uuid :: String
    , modules :: L.List String
    }

instance decodeChannelInfo :: DecodeJson ChannelInfo where
  decodeJson json = do
    obj <- decodeJson json
    uuid <- obj .? "channelUUID"
    modules <- obj .? "channelModules"
    pure $ ChannelInfo { uuid: uuid, modules: modules }

instance encodeChannelInfo :: EncodeJson ChannelInfo where
  encodeJson (ChannelInfo info)
     = "channelUUID" := info.uuid
    ~> "channelModules" := info.modules
