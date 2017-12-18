module Tabletop.Message.Ur where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.Tuple (uncurry)
import Tabletop.Message (ChannelInfo, SystemMessage, TabletopMessage, TabletopResponse)

--- UrMessage ---

type UrTabletopMessage = TabletopMessage SystemMessage String UrMessage

data UrMessage
  = Join Player
  | Move Int
  | PassTurn
  | GetCurrentGame

instance encodeUrMessage :: EncodeJson UrMessage where
  encodeJson = case _ of
    Join player ->
      "tag" := "Join"
      ~> "contents" := encodeJson player
      ~> jsonEmptyObject
    Move n ->
      "tag" := "Move"
      ~> "contents" := encodeJson n
      ~> jsonEmptyObject
    PassTurn ->
      "tag" := "PassTurn"
      ~> jsonEmptyObject
    GetCurrentGame ->
      "tag" := "GetCurrentGame"
      ~> jsonEmptyObject

data Player
  = PlayerBlack
  | PlayerWhite

instance encodePlayer :: EncodeJson Player where
  encodeJson = case _ of
    PlayerBlack ->
      fromString "PlayerBlack"
    PlayerWhite ->
      fromString "PlayerWhite"

instance decodePlayer :: DecodeJson Player where
  decodeJson json = case decodeJson json of
    Right str -> case str of
      "PlayerBlack" -> Right PlayerBlack
      "PlayerWhite" -> Right PlayerWhite
      _ -> Left $ "Expected Player: " <> str
    Left str -> Left $ "Expected JsonString: " <> str

showPlayer :: Player -> String
showPlayer PlayerBlack = "Player Black"
showPlayer PlayerWhite = "Player White"

data Turn
  = BlackTurn
  | WhiteTurn

derive instance eqTurn :: Eq Turn

instance decodeTurn :: DecodeJson Turn where
  decodeJson json = case decodeJson json of
    Right str -> case str of
      "Black" -> Right BlackTurn
      "White" -> Right WhiteTurn
      _ -> Left $ "Expected Turn: " <> str
    Left str -> Left $ "Expected JsonString: " <> str

showTurn :: Turn -> String
showTurn BlackTurn = "Black"
showTurn WhiteTurn = "White"

newtype UrBoard = UrBoard
  { black :: Side
  , white :: Side
  , turn :: Turn
  , dice :: Int
  }

instance decodeUrBoard :: DecodeJson UrBoard where
  decodeJson json = do
    obj <- decodeJson json
    black <- obj .? "black"
    white <- obj .? "white"
    turn <- obj .? "turn"
    dice <- obj .? "dice"
    pure $ UrBoard { black, white, turn, dice }

newtype Side = Side
  { pieces :: Int
  , path :: Array Int
  , scored :: Int
  }

instance decodeSide :: DecodeJson Side where
  decodeJson json = do
    obj <- decodeJson json
    pieces <- obj .? "pieces"
    path <- obj .? "path"
    scored <- obj .? "scored"
    pure $ Side { pieces, path, scored }

--- UrResponse ---

type UrTabletopResponse = TabletopResponse ChannelInfo UrResponse

data UrResponse
  = JoinSuccess Player
  | JoinSuccessOther Player
  | JoinFailure String
  | MoveSuccess UrBoard
  | MoveFailure String
  | PassSuccess NextTurn
  | PassFailure String
  | CurrentGame UrBoard
  | PlayerHasWon Player

instance decodeUrResponse :: DecodeJson UrResponse where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    contents <- obj .? "contents"
    case tag of
      "JoinSuccess" -> map JoinSuccess $ decodeJson contents
      "JoinSuccessOther" -> map JoinSuccessOther $ decodeJson contents
      "JoinFailure" -> map JoinFailure $ decodeJson contents
      "MoveSuccess" -> map MoveSuccess $ decodeJson contents
      "PassSuccess" -> map PassSuccess $ decodeJson contents
      "PassFailure" -> map PassFailure $ decodeJson contents
      "CurrentGame" -> map CurrentGame $ decodeJson contents
      "PlayerHasWon" -> map PlayerHasWon $ decodeJson contents
      str -> Left $ "Expected UrTabletopResponse: " <> str

data NextTurn = NextTurn Turn Int

instance decodeNextTurn :: DecodeJson NextTurn where
  decodeJson json =
    map (uncurry NextTurn) $ decodeJson json
