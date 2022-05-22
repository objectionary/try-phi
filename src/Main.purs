module Main where

import Prelude

import App.Button as B
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

-- data Action = Increment | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> B.Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> B.Increment ] [ HH.text "+" ]
      , HH.button [ HE.onClick \_ -> B.Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    B.Increment -> H.modify_ \state -> state + 1
    B.Decrement -> H.modify_ \state -> state - 1