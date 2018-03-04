module Tabletop.Connection where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA

-- TODO: We may want to find something different for handling json or possibly
-- even using graphql if we can.
wsSender :: forall eff a. EncodeJson a
         => WS.WebSocket -> CR.Consumer a (Aff (HA.HalogenEffects (dom :: DOM | eff))) Unit
wsSender socket = CR.consumer $ \msg -> do
  liftEff $ WS.sendString socket $ stringify $ encodeJson msg
  pure Nothing

wsProducer :: forall eff. WS.WebSocket -> CR.Producer String (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener WSET.onMessage (listener emit) false (WS.socketToEventTarget socket)
  where
    listener emit = EET.eventListener \ev -> do
      for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          emit (Left msg)

    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< toForeign

-- FIXME: This type is tough to decipher and should be cleaned up for others.
-- Using it isn't too bad and we have plenty of examples on how to do so.
wsConsumer :: forall t71 t78 t79 eff. DecodeJson t79 => (t78 ~> Aff (HA.HalogenEffects eff)) -> (Either String t79 -> (Unit -> t78 Unit)) -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query fn = CR.consumer \msg -> do
  query $ H.action $ fn (decodeJson =<< jsonParser msg)
  pure Nothing
