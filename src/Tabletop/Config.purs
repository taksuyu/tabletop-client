module Tabletop.Config where

import Data.Semigroup ((<>))

tabletopDomainAPI :: String
tabletopDomainAPI = "api.tabletop.quiet.space"

tabletopWebSocketProtocol :: String
tabletopWebSocketProtocol = "wss://" <> tabletopDomainAPI

tabletopUrWebSocket :: String
tabletopUrWebSocket = tabletopWebSocketProtocol  <> "/games/ur"
