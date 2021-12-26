{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String

import qualified Phi.Minimal                      as Phi

import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import qualified Phi.Minimal.ConfigurationDot as CDot

import           Data.Graph.Inductive.PatriciaTree (Gr)

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Data.Map.Lazy as Map

#endif

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#else
runApp :: IO () -> IO ()
runApp = id
#endif

data Model = Model
  { modelSource :: MisoString
  , modelAST    :: Either String Phi.Term
  , graphStepNumber :: Int
  , reloads :: Integer
  } deriving (Show, Eq)

-- | Sum type for application events
data Action
  = Recompile MisoString
  | Reload
  | NoOp
  | NextStep
  | PrevStep
  deriving (Show, Eq)

initModel :: Model
initModel = Model
  { modelSource = ""
  , modelAST = Left "initializing..."
  , graphStepNumber = 0
  , reloads = 0
  }

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = Reload        -- initial action to be executed on application load
    model  = initModel            -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Just "__app__"   -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Reload m = m {reloads = reloads m + 1} <# do
  Recompile <$> codemirrorGetValue
updateModel (Recompile code) m = noEff m
  { modelAST = Phi.parseTerm (fromMisoString code) }
updateModel NoOp m = noEff m

-- TODO optimize
-- control step number
updateModel PrevStep m@Model{..} =
  noEff m { graphStepNumber = max 0 (graphStepNumber - 1) }
updateModel NextStep m@Model{..} =
  noEff m { graphStepNumber = min (graphStepNumber + 1) (Prelude.length (getGraphSteps m) - 1)}

getGraphSteps :: Model -> [CGraph.Configuration Gr]
getGraphSteps Model{..} =
  case modelAST of
        Left _ -> []
        Right term ->
          CGraph.steps @Gr (CGraph.initConfiguration term)

tagId :: String -> Attribute action
tagId s = id_ $ ms @String s

infoIcon :: MisoString -> MisoString -> View action
infoIcon i content =  i_ [
  id_ i,
  class_ "bi bi-info-square",
  data_ "bs-container" "body",
  data_ "bs-toggle" "popover",
  data_ "bs-placement" "top",
  data_ "bs-content" content] []

-- infos :: Map.Map MisoString MisoString
-- infos = Map


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model{..} =
  case modelAST of
    Left err -> div_ []
      [ button_ [ onClick Reload] [ text "Reload" ]
      , br_ []
      , pre_ [] [ text (ms err) ]
      ]
    Right term -> div_ [id_ "app_div", Miso.name_ $ toMisoString ("reloads_" ++ show reloads)]
      [ button_ [ onClick Reload ] [ text "Reload" ]
      , br_ []
      , br_ []
      , table_ [] [ tr_ []
        [ td_ []
            [ div_ [] []
            , pre_ [] [text "Original term:", infoIcon "original_term" "<b>original_term<b>"]
            , pre_ [] [text (ms (show term))] ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ div_ [] [text "Weak head normal form (WHNF):"]
            , pre_ [] [text (ms (show (Phi.whnf term)))] ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ div_ [] [text "Normal form (NF):"]
            , pre_ [] [text (ms (show (Phi.nf term)))] ]
        ] ]
      , table_ [] [ tr_ []
        [ td_ []
            [ details_ [] [
              summary_ [] [text "Call-by-name term reduction:"]
            , pre_ [] [text . ms . show $ Phi.ppWhnfSteps term]] ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ details_ [] [
              summary_ [] [text "Call-by-name term reduction (via abstract machine):"]
            , pre_ [] [text . ms . show $ Phi.ppStepsFor term]] ]
        ] ]
      , table_ [] [
          tr_ [] [
            td_ [] [
              tr_ [] [
                button_ [ onClick PrevStep ] [ text "Previous step" ]
              , button_ [ onClick NextStep ] [ text "Next step" ]
              ]
            , tr_ [] [
                div_ [] [
                  text "Call-by-name evaluation on a graph:"
                  ]
              , pre_ [] [text . ms . show $ Phi.ppGraphStepsFor term graphStepNumber]
              ]
            ]
          , td_ [ width_ "50" ] [ ]
          , td_ [] [
              img_ [
                let
                  dotStringState = CDot.renderAsDot @Gr ((getGraphSteps m) !! graphStepNumber)
                in
                  src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> dotStringState ))
                  , height_ "400"
              ]
            ]
          , td_ [] [
              details_ [] [
                summary_ [tagId "dot_string"] [text "Graph DOT string"]
              , pre_ [] [ text (ms (Phi.renderAsColorfulDot term)) ]]
            ]
          ]
        ]
      ]


#ifndef __GHCJS__
codemirrorGetValue :: JSM MisoString
codemirrorGetValue = return (ms (show Phi.ex6))
#else
foreign import javascript unsafe "$r = view.state.doc.toString();"
  codemirrorGetValue :: IO MisoString
#endif
