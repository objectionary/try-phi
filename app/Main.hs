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
import Content (infoContent)

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle

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
#ifndef __GHCJS__
    mountPoint = Nothing
#else
    mountPoint = Just "ghcjs"   -- mount point for application (Nothing defaults to 'body')
#endif
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

infoIcon :: MisoString -> View action
infoIcon infoId =  i_ [
  id_ infoId,
  class_ "bi bi-info-square",
  data_ "bs-container" "body",
  data_ "bs-toggle" "popover",
  data_ "bs-placement" "top",
  data_ "bs-content" (infoContent infoId)] []

data TabMode = Active | Disabled

tabButton :: MisoString -> MisoString -> MisoString -> MisoString -> TabMode  -> View action
tabButton buttonId contentId infoId txt isActive =
  button_ [
    class_ $ "nav-link" <> active,
    id_ buttonId,
    data_ "bs-toggle" "tab",
    data_ "bs-target" $ toMisoString ("#" <> contentId),
    type_ "button",
    textProp "role" "tab",
    textProp "aria-controls" $ toMisoString contentId,
    textProp "aria-selected" selected
    ] [
    infoIcon infoId,
    text txt
  ]
  where 
    (active, selected) = 
      case isActive of 
        Active -> (" active", "true") 
        _ -> ("", "false")


tabContent :: MisoString -> View action -> MisoString -> TabMode -> View action
tabContent tabId content buttonId isActive =
  div_ [
    class_ $ "tab-pane fade" <> active, 
    class_ "pt-3",
    id_ tabId, 
    textProp "role" "tabpanel", 
    textProp "aria-labelledby" buttonId
  ] [
    content
  ]
  where 
    active = 
      case isActive of 
        Active -> " show active"
        _ -> ""

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model{..} =
  case modelAST of
    Left err -> div_ []
      [ button_ [ onClick Reload] [ text "Reload" ]
      , br_ []
      , pre_ [] [ text (ms err) ]
      ]
    Right term -> 
      div_ [] [
        link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", rel_ "stylesheet", type_ "text/css"]
      , script_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js", type_ "text/javascript"] ""
      , link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.3.0/font/bootstrap-icons.css", rel_ "stylesheet", type_ "text/css"]
      , link_ [href_ "styles.css", rel_ "stylesheet", type_ "text/css"]
      , div_ [
          id_ "app_div", 
          Miso.name_ $ toMisoString ("reloads_" ++ show reloads)
        ] [ 
          button_ [ onClick Reload] [ text "Reload" ]
        , nav_ [] [
            div_ [class_ "nav nav-tabs", id_ "nav-tab", textProp "role" "tablist"] [
              tabButton "button_original_term" "content_original_term" "info_original_term" " Original term" Active
            , tabButton "button_whnf" "content_whnf" "info_whnf" " Weak head normal form (WHNF)" Disabled
            , tabButton "button_nf" "content_nf" "info_nf" " Normal form (NF)" Disabled
            , tabButton "button_cbn_reduction" "content_cbn_reduction" "info_cbn_reduction" " Call-by-name term reduction" Disabled
            , tabButton "button_cbn_with_tap" "content_cbn_with_tap" "info_cbn_with_tap" " Call-by-name term reduction (via abstract machine)" Disabled
            , tabButton "button_cbn_with_graph" "content_cbn_with_graph" "info_cbn_with_graph" " Call-by-name evaluation on a graph" Disabled
            ]
          ]
        , div_ [class_ "tab-content", id_"nav-tabContent"] [
            tabContent "content_original_term" (pre_ [] [text (ms (show term))])  "button_original_term" Active
          , tabContent "content_whnf" (pre_ [] [text (ms (show (Phi.whnf term)))]) "button_whnf" Disabled
          , tabContent "content_nf" (pre_ [] [text (ms (show (Phi.nf term)))]) "button_nf" Disabled
          , tabContent "content_cbn_reduction" (pre_ [] [text . ms . show $ Phi.ppWhnfSteps term]) "button_cbn_reduction" Disabled
          , tabContent "content_cbn_with_tap" (pre_ [] [text . ms . show $ Phi.ppStepsFor term]) "button_cbn_with_tap" Disabled
          , tabContent "content_cbn_with_graph" (graphContent term) "button_cbn_with_graph" Disabled
          ]
        ]
      ]
    where
      graphContent term =
        div_ [class_ "row"] [
          div_ [class_ "col"] [
            div_ [class_ "row"] [
              div_ [class_ "col"] [
                button_ [ onClick PrevStep ] [ text "Previous step" ]
              , button_ [ onClick NextStep ] [ text "Next step" ]
              ]
            , div_ [class_ "col-2"] []
            ]
          , pre_ [] [text . ms . show $ Phi.ppGraphStepsFor term graphStepNumber]
          ]
        , div_ [class_ "col"] [
            img_ [
              let
                dotStringState = CDot.renderAsDot @Gr ((getGraphSteps m) !! graphStepNumber)
              in
                src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> dotStringState ))
                , height_ "400"
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
