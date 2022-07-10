module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Phi (component)
import Utils (setGlobalBoolean)
import Web.HTML (window) as Web
import Web.HTML.Location (host) as Web
import Web.HTML.Window (location) as Web

main :: Effect Unit
main =
  do
    host <- Web.window >>= Web.location >>= Web.host
    setGlobalBoolean "dev" (host == "localhost:1234")
    HA.runHalogenAff do
      body <- HA.awaitBody
      runUI component unit body
