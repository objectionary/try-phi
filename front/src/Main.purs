module Main
  ( main
  ) where

import Prelude

import Data.String (length, take)
import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Phi (component, globalVarDev)
import Utils (setGlobalBoolean)
import Web.HTML (window) as Web
import Web.HTML.Location (host) as Web
import Web.HTML.Window (location) as Web

main :: Effect Unit
main =
  do
    host <- Web.window >>= Web.location >>= Web.host
    let 
      localhost = "localhost"
      isLocalhost = (take (length localhost) host == localhost)
    log $ show isLocalhost
    setGlobalBoolean globalVarDev isLocalhost
    HA.runHalogenAff do
      body <- HA.awaitBody
      runUI component unit body