{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Miso
import           Miso.String

import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import           Data.Graph.Inductive.PatriciaTree    (Gr)
import qualified Phi.Minimal                          as Phi

import qualified Phi.Minimal.ConfigurationDot         as CDot
import Content(TabMode(..), tabButton, tabContent)


#ifndef __GHCJS__
import           Language.Javascript.JSaddle          (eval,
                                                       strToText, textToStr,
                                                       valToStr)
import           Language.Javascript.JSaddle.Warp     as JSaddle
#endif

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
-- runApp f = do
--   bString <- B.readFile "bundle.js"
--   jSaddle <- JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) (JSaddle.jsaddleAppWithJs (B.append (JSaddle.jsaddleJs False) bString))
--   Warp.runSettings (Warp.setPort 8082 (Warp.setTimeout 3600 Warp.defaultSettings)) jSaddle
#else
runApp :: IO () -> IO ()
runApp = id
#endif

data Model = Model
  { modelSource     :: MisoString
  , modelAST        :: Either String Phi.Term
  , graphStepNumber :: Int
  , getCodeScript :: String
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
  , getCodeScript = ""
  }

-- | Entry point for a miso application
main :: IO ()
main = do
  getCodeScript <- readFile "src/scripts/get-code.js"
  let model = initModel {getCodeScript = getCodeScript}
  runApp $ startApp App {..}
  where
    initialAction = NoOp        -- initial action to be executed on application load
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
updateModel Reload m = m <# do
  Recompile <$> codemirrorGetValue m
updateModel (Recompile code) m = noEff m
  { modelAST = Phi.parseTerm (fromMisoString code) }
updateModel NoOp m = noEff m
updateModel PrevStep m@Model{..} =
  noEff m { graphStepNumber = max 0 (graphStepNumber - 1) }
updateModel NextStep m@Model{..} =
  noEff m { graphStepNumber = min (graphStepNumber + 1) (Prelude.length (getGraphSteps m) - 1)}

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model{..} =
      div_ [] [
        div_ [id_ "editor"] []
      , link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", rel_ "stylesheet", type_ "text/css"]
      , script_ [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js", type_ "text/javascript"] ""
      , script_ [src_ "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-minimal-editor/docs/build/bundle.js", type_ "text/javascript"] ""
      , link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.3.0/font/bootstrap-icons.css", rel_ "stylesheet", type_ "text/css"]
      , link_ [href_ "styles.css", rel_ "stylesheet", type_ "text/css"]
      , div_ [
          id_ "app_div"
        ] [
          button_ [ onClick Reload, class_ "btn btn-secondary mb-5"] [ text "Reload" ]
        , case modelAST of
            Left err   -> errorTabs $ ms err
            Right term -> termTabs term m
        ]
      ]

getGraphSteps :: Model -> [CGraph.Configuration Gr]
getGraphSteps Model{..} =
  case modelAST of
        Left _ -> []
        Right term ->
          CGraph.steps @Gr (CGraph.initConfiguration term)

termTabs :: Phi.Term -> Model -> View Action
termTabs term m@Model{..} =
    div_ [] [
      nav_ [] [
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
    where
      graphContent t =
        div_ [class_ "row"] [
          div_ [class_ "col"] [
            div_ [class_ "row"] [
              div_ [class_ "col"] [
                button_ [ onClick PrevStep ] [ text "Previous step" ]
              , button_ [ onClick NextStep ] [ text "Next step" ]
              ]
            , div_ [class_ "col-2"] []
            ]
          , pre_ [] [text . ms . show $ Phi.ppGraphStepsFor t graphStepNumber]
          ]
        , div_ [class_ "col"] [
            img_ [
              let
                dotStringState = CDot.renderAsDot @Gr (getGraphSteps m !! graphStepNumber)
              in
                src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> dotStringState ))
                , height_ "400"
            ]
          ]
        ]

errorTabs :: MisoString -> View Action
errorTabs err =
  div_ [onCreated Reload] [
      nav_ [] [
        div_ [class_ "nav nav-tabs", id_ "nav-tab", textProp "role" "tablist"] [
          tabButton "button_error" "content_error" "info_error" " Error" Active
        ]
      ]
    , div_ [class_ "tab-content", id_"nav-tabContent"] [
        tabContent "content_error" (pre_ [] [text $ ms err])  "button_error" Active
      ]
  ]

-- func :: String
-- func = 


#ifndef __GHCJS__
codemirrorGetValue :: Model -> JSM MisoString
-- codemirrorGetValue = return (ms (show Phi.ex6))
codemirrorGetValue Model{..} = do
  res <- eval $ textToStr (pack getCodeScript)
  str <- valToStr res
  let misoStr = ms $ strToText str
  return misoStr

#else
foreign import javascript unsafe "$r = app.view.state.doc.toString();"
  codemirrorGetValue :: IO MisoString
#endif
