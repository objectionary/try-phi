module Main
  ( component
  , getState
  , md1
  , main
  , Request
  ) where

import Prelude

import Data.Array (head)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HE
import Halogen.VDom.Driver (runUI)
import Phi (Action(..), Editor(..), State(..), Tab(..), TabId(..), html)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

md1 :: State
md1 =
  State
    { currentEditor: EOEditor
    , graphStep: 1
    , tabs:
        ( \( Tuple
              ( Tuple a b
            )
              c
          ) ->
            Tab { id: a, buttonText: b, isActive: c, tabContent: HE.text b }
        )
          <$> DA.zip (DA.zip ids btexts) isActives
    }
  where
  ids = [ TEO, TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP, TCBNWithGraph ]

  btexts = [ "EO code", "Phi term", " WHNF", "NF", "CBN Reduction", "CBN With TAP", "CBN With Graph" ]

  isActives = [ true, false, false, false, false, false, false ]

initTab = Tab {id: TEO, buttonText: "EO code", isActive: true, tabContent: HE.text "EO code"  }

data Request
  = Request
    { code :: String
    , editor :: Editor
    }

-- FIXME make a request to a server
getState :: Request -> State
getState s = md1

component :: ∀ a b c d. Component a b c d
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just (SelectTab initTab)
      }
    }
  where
  initialState _ = md1

  render s = html s

  -- (ParseError $ EOParseError "noo")
  -- HH.div_
  --   [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
  --   , HH.div_ [ HH.text $ show state ]
  --   , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
  --   ]
  handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    SelectTab t1 ->
      H.modify_
        $ \state -> case state of
            State s ->
              State
                ( s
                    { tabs = (\t2@(Tab t') -> Tab (t' { isActive = t1 == t2})) <$> s.tabs
                    }
                )
            s -> s
    NextStep ->
      H.modify_
        $ \state -> case state of
            State s -> State $ s { graphStep = s.graphStep + 1 }
            s -> s
    PrevStep ->
      H.modify_
        $ \state -> case state of
            State s -> State $ s { graphStep = s.graphStep - 1 }
            s -> s
    SelectCurrentEditor e ->
      H.modify_
        $ \state -> case state of
            State s -> State $ s { currentEditor = e }
            s -> s
    Recompile c -> H.modify_ $ \_ -> getState (Request { code: c, editor: EOEditor })
