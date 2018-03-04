module Ur where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (href, setHash)
import DOM.HTML.Window (location)
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Websocket.WebSocket as WS
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.String as S
import Data.Tuple (Tuple(..))
import Data.URI (Fragment(..), URI(..))
import Data.URI.URI (parse)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Tabletop.Config
import Tabletop.Connection
import Tabletop.Message
import Tabletop.Message.Ur

data UrQuery a
  = MovePiece Int a
  | ConfirmMove UrBoard a

  | Pass a
  | ConfirmPass NextTurn a

  | StartGame a
  | JoinGame String a

  | BecomePlayer Player a
  | ConfirmBecomePlayer Player a

  | SetConnection ChannelInfo a
  | SetGame UrBoard a
  | SetMessage UserMessage a

data UserMessage
  = Error String
  | Info String

instance showUserMessage :: Show UserMessage where
  show (Error str) = "Error: " <> str
  show (Info str) = "Info: " <> str

type UrState =
  { mode :: Mode
  , error :: String
  , connection :: Maybe ChannelInfo
  , joinInfo :: Maybe String
  }

data Mode
  = GameInProgress PlayerMode UrBoard
  | StandBy

data PlayerMode
  = PlayerMode Player
  | Spectator

initialState :: Maybe String -> UrState
initialState i =
  { mode: StandBy
  , error: ""
  , connection: Nothing
  , joinInfo: i
  }

render :: UrState -> H.ComponentHTML UrQuery
render state = HH.div_
  [ HH.h1_ [ HH.text "The Game of Ur" ]
  , renderScene state
  ]

renderScene :: UrState -> H.ComponentHTML UrQuery
renderScene state =
  let renderState = case state.mode of
        StandBy ->
          renderStandBy state
        GameInProgress playerMode board ->
          renderGame board
  in HH.div_ $
    if not $ S.null state.error
      then
        [ HH.p_ [ HH.text state.error ]
        , renderState
        ]
      else
        [ renderState ]

renderStandBy :: UrState -> H.ComponentHTML UrQuery
renderStandBy state = HH.div_ $
  [ HH.button [ HE.onClick (HE.input_ StartGame)] [ HH.text "Start game" ] ]
  <>
  maybe [] (\ uuid -> [ HH.button [ HE.onClick (HE.input_ $ JoinGame uuid)] [HH.text "Join game" ] ] )
    state.joinInfo

renderGame :: UrBoard -> H.ComponentHTML UrQuery
renderGame (UrBoard{ black: black@Side{ scored: blackScored }, white: white@Side{ scored: whiteScored }, turn, dice }) =
  HH.div_
  [ -- Score
    HH.p_ [ HH.text ("Black: " <> show blackScored <> "White: " <> show whiteScored)]

    -- Game board
  , HH.div [ HP.class_ (HH.ClassName "ur-game") ]
    [ HH.div [ HP.class_ $ HH.ClassName "lane side side-black" ]
      [ HH.div_ blackSide.start
      , HH.div_ blackSide.end
      ]
    , HH.div [ HP.class_ (HH.ClassName "midLane") ] middlePath
    , HH.div [ HP.class_ $ HH.ClassName "lane side side-white" ]
      [ HH.div_ whiteSide.start
      , HH.div_ whiteSide.end
      ]
    ]

    -- Turn information
  , HH.div_
    [ HH.p_ [ HH.text $ "Turn: " <> showTurn turn ]
    , HH.p_ [ HH.text $ "Roll: " <> show dice ]
    , HH.button [ HE.onClick (HE.input_ Pass) ] [ HH.text "Pass" ]
    ]

  , HH.div_
    [ HH.p_ [ HH.text "Join as player"]
    , HH.button [HE.onClick (HE.input_ $ BecomePlayer PlayerBlack)] [ HH.text "Black"]
    , HH.button [HE.onClick (HE.input_ $ BecomePlayer PlayerWhite)] [ HH.text "White"]
    ]
  ]
  where
    blackSide = f PlayerBlack black
    whiteSide = f PlayerWhite white

    middlePathSquare n =
      case Tuple (A.find (\c -> n == c) blackSide.middle) (A.find (\c -> n == c) whiteSide.middle) of
        Tuple (Just i) Nothing ->
          square PlayerBlack blackSide.middle i
        Tuple Nothing (Just i) ->
          square PlayerWhite whiteSide.middle i
        _ ->
          HH.div [ boardAttrs n ] []

    -- NOTE: We aren't checking that the numbers we got from the server are in order.
    middlePath =
      foldr (\ a b -> middlePathSquare a A.: b) [] (5 A... 12)

    boardAttrs n =
      let classNames str = HP.class_ $ HH.ClassName str
      in if n == 4 || n == 8 || n == 14
         then
           classNames "ur-board ur-placemarker"
         else
           classNames "ur-board"

    -- f :: forall p i. Player -> Side -> { start :: [HH.HTML p i], middle :: [HH.HTML p i], end :: [HH.HTML p i] }
    f player (Side{ path }) =
      let starts = A.partition (\a -> a < 5 && a > 0) path
          middles = A.partition (\a -> a < 13 && a > 4) starts.no
          ends = A.partition (\a -> a < 15 && a > 12) middles.no
      in { start: A.reverse $ squares player starts.yes (1 A... 4)
         , middle: middles.yes
         , end: A.reverse $ squares player ends.yes (13 A... 14)
         }

    square player array n =
      let p = piece player $ A.find (\c -> n == c) array
      in if not p.hasPiece && n == dice
         then
           HH.div
           [ HE.onClick $ HE.input_ $ MovePiece n
           , boardAttrs n
           ]
           [p.html]
         else
           HH.div [ boardAttrs n ] [p.html]

    squares player array ns =
      foldr (\ a b -> square player array a A.: b ) [] ns

    -- piece :: Player -> [Int] -> Maybe Int -> (HTML p i, Boolean)
    piece player p =
      let playerAttr = case player of
            PlayerBlack -> "piece black"
            PlayerWhite -> "piece white"
      in case p of
        Just i ->
          { html: HH.div
            [ HE.onClick $ HE.input_ $ MovePiece i
            , HP.class_ $ HH.ClassName playerAttr
            ]
            []
          , hasPiece: true
          }
        Nothing ->
          { html: HH.div_
            []
          , hasPiece: false
          }

ui :: forall eff. H.Component HH.HTML UrQuery (Maybe String) UrTabletopMessage (Aff (HA.HalogenEffects (console :: CONSOLE | eff)))
ui = H.component
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }

eval :: forall eff. UrQuery ~> H.ComponentDSL UrState UrQuery UrTabletopMessage (Aff (HA.HalogenEffects (console :: CONSOLE | eff)))
eval = case _ of
  MovePiece p next -> do
    state <- H.get
    case state.connection of
      Just (ChannelInfo{ uuid }) ->
        H.raise $ ServiceMessage uuid "UrService" (Move p)
      Nothing ->
        H.modify (_ { error = show $ Error "No valid service information." })
    pure next
  ConfirmMove board next -> do
    state <- H.get
    case state.mode of
      GameInProgress pm _ ->
        H.modify (_ { mode = GameInProgress pm board })
      _ ->
        H.modify (_ { error = show $ Error "Game is not in progress, but received a move from server." })
    pure next
  Pass next -> do
    state <- H.get
    case state.connection of
      Just (ChannelInfo{ uuid }) ->
        H.raise $ ServiceMessage uuid "UrService" $ PassTurn
      Nothing ->
        H.modify (_ { error = show $ Error "No valid service information." })
    pure next
  ConfirmPass (NextTurn nt nd) next -> do
    state <- H.get
    case state.mode of
      GameInProgress pm (UrBoard r) ->
        H.modify (_ { mode = GameInProgress pm (UrBoard $ r { turn = nt, dice = nd }) })
      _ ->
        H.modify (_ { error = show $ Error "Game is not in progress, but received a pass from server." })
    pure next
  StartGame next -> do
    H.raise $ SystemMessage CreateGame
    pure next
  JoinGame uuid next -> do
    H.liftAff $ log "about to send"
    H.raise $ ServiceMessage uuid "UrService" $ GetCurrentGame
    H.modify (_ { connection = Just $ ChannelInfo { uuid, modules: mempty } })
    pure next
  BecomePlayer player next -> do
    state <- H.get
    case state.connection of
      Just (ChannelInfo{ uuid }) ->
        H.raise $ ServiceMessage uuid "UrService" $ Join player
      Nothing ->
        H.modify (_ { error = show $ Error "No valid service information." })
    pure next
  ConfirmBecomePlayer player next -> do
    state <- H.get
    case state.mode of
      GameInProgress pm board ->
        H.modify (_ { mode = GameInProgress (PlayerMode player) board })
      _ ->
        H.modify (_ { error = show $ Error "Game is not in progress, but received a join from server. "})
    pure next
  SetConnection chanInfo@(ChannelInfo { uuid }) next -> do
    H.modify (_ { connection = Just chanInfo })
    H.raise $ ServiceMessage uuid "UrService" $ GetCurrentGame
    pure next
  SetGame board next -> do
    H.modify (_ { mode = GameInProgress Spectator board })
    state <- H.get
    case state.connection of
      Just (ChannelInfo{ uuid }) ->
        H.liftEff $ do
          window
          >>= location
          >>= setHash uuid
      Nothing ->
        H.modify (_ { error = show $ Error "Game UUID is not set." })
    pure next
  SetMessage message next -> do
    H.modify (_ { error = show message })
    pure next

main :: forall eff. Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM | eff)) Unit
main = do
  ws <- WS.create (WS.URL tabletopUrWebSocket) []
  w <- window
  loc <- location w
  uri <- href loc
  HA.runHalogenAff do
    element <- do
      sElement <- HA.selectElement (QuerySelector "#content")
      body <- HA.awaitBody
      case sElement of
        Just content -> pure content
        _            -> pure body

    io <- runUI ui (
      case parse uri of
        Right (URI _ _ _ (Just (Fragment f))) ->
          Just f
        _ ->
          Nothing
      ) element

    io.subscribe $ wsSender ws

    CR.runProcess (wsProducer ws CR.$$ urConsumer io.query)

urConsumer :: forall eff. (UrQuery ~> Aff (HA.HalogenEffects eff)) -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
urConsumer query = wsConsumer query $ \msg ->
  case msg of
    Right serverMsg -> case serverMsg of
      SystemResponse chanInfo ->
        SetConnection chanInfo
      ServiceResponse r -> case r of
        CurrentGame board ->
          SetGame board
        JoinSuccess player ->
          ConfirmBecomePlayer player
        JoinSuccessOther player ->
          SetMessage $ Info (showPlayer player <> " has joined!")
        -- TODO: If a join fails we should reset the state
        JoinFailure str ->
          SetMessage $ Info str
        MoveSuccess board ->
          ConfirmMove board
        MoveFailure str ->
          SetMessage $ Info str
        PassSuccess nextTurn ->
          ConfirmPass nextTurn
        PassFailure str ->
          SetMessage $ Info str

        -- TODO: Change the state or allow users to start a new game.
        PlayerHasWon player ->
          SetMessage $ Info (showPlayer player <> " has won!")
    Left str -> SetMessage $ Error str
