{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import           Site.Content                              (TabMode (..), infoIcon,
                                                       tabButton, tabContent)
import           Data.Graph.Inductive.PatriciaTree    (Gr)
import           Data.Map.Strict.Internal             as Map (fromList)
import           Data.Text                            (pack)
import           Miso
import           Miso.String                          (MisoString,
                                                       fromMisoString, ms)
import qualified Phi.Minimal                          as Phi (Term (..), nf,
                                                              parseTerm,
                                                              ppGraphStepsFor,
                                                              ppStepsFor,
                                                              ppWhnfSteps, whnf)
import qualified Phi.Minimal.ConfigurationDot         as CDot
import           Phi.Minimal.EO.Pretty                (ppTerm)
import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import qualified Phi.Minimal.Model                    as Model (ex19)
import qualified Data.ByteString.Lazy as B
import qualified Network.Wai.Handler.Warp         as Warp
-- import qualified Network.WebSockets as WS

#ifndef __GHCJS__
import           Language.Javascript.JSaddle          (eval, strToText,
                                                       textToStr, valToStr)
import           Language.Javascript.JSaddle.Warp     as JSaddle
import Data.Text.Lazy (unpack)
#endif

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.run 8080 f
  -- do
  -- bString <- B.readFile "editor/docs/build/bundle.js"
  -- jSaddle <- JSaddle.jsaddleOr WS.defaultConnectionOptions (f >> syncPoint) (JSaddle.jsaddleAppWithJs (B.append (JSaddle.jsaddleJs False) bString))
  -- Warp.runSettings (Warp.setPort 8082 (Warp.setTimeout 3600 Warp.defaultSettings)) jSaddle

#else
runApp :: IO () -> IO ()
runApp = id
#endif

data Model = Model
  { modelSource      :: MisoString,
    modelAST         :: Either String Phi.Term,
    graphStepNumber  :: Int,
    jsGetCode        :: String,
    popoversScript   :: String,
    setSnippetScript :: String
  }
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = Recompile MisoString
  | Reload
  | NoOp
  | NextStep
  | PrevStep
  deriving (Show, Eq)

initModel :: Model
initModel =
  Model
    { modelSource = "",
      modelAST = Right Model.ex19,
      graphStepNumber = 0,
      jsGetCode = "",
      popoversScript = "",
      setSnippetScript = ""
    }

initApp :: Model -> App Model Action
initApp model = Miso.App {
  initialAction = NoOp, -- initial action to be executed on application load
  update = updateModel, -- update function
  view = viewModel, -- view function
  events = defaultEvents, -- default delegated events
  subs = [], -- empty subscription list
  mountPoint = Nothing,
  logLevel = Off, -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)
  model = model
}

-- | Entry point for a miso application
main :: IO ()
main = do
#ifndef __GHCJS__
  jsGetCode <- readFile "src/Site/scripts/get-code.js"
  -- popoversScript <- readFile "src/scripts/init-popovers.js"
  -- setSnippetScript <- readFile "src/scripts/set-snippet.js"
  let model = initModel {
    jsGetCode = jsGetCode
    -- popoversScript = popoversScript,
    -- setSnippetScript = setSnippetScript
  }
#else
  let model = initModel
#endif
  runApp $ startApp (initApp model)
  
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Reload m =
  m <# do

#ifndef __GHCJS__
    Recompile <$> codemirrorGetValue m
#else
    Recompile <$> codemirrorGetValue
#endif

updateModel (Recompile code) m =
  noEff
    m
      { modelAST = Phi.parseTerm (fromMisoString code)
      }
updateModel NoOp m = noEff m
updateModel PrevStep m@Model {..} =
  noEff m {graphStepNumber = max 0 (graphStepNumber - 1)}
updateModel NextStep m@Model {..} =
  noEff m {graphStepNumber = min (graphStepNumber + 1) (Prelude.length (getGraphSteps m) - 1)}



-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model {..} =
  div_
    []
    [ eoLogoSection,
      editorDiv,
      cdns,
      div_
        [ id_ "app_div"
        , class_ "pt-5"
        ]
        [ button_ [onClick Reload, class_ "btn btn-secondary mb-5"] [text "Reload"],
          let
            (term, divElem) =
              case modelAST of
                Left err   -> (Phi.Loc 0, div_ [class_ "pb-2"] [pre_ [] [text $ ms err]])
                Right t -> (t, div_ [][])
          in
            div_ [] [divElem, termTabs term m {modelAST = Right term}]
        ],
      pageFooter
      ,
      div_ [onCreated Reload] []
    ]

pageFooter :: View action
pageFooter =
  nav_
    []
    [ ul_
        []
        [ li_
            []
            [ text "To discuss how 𝜑-calculus works, join our ",
              a_ [href_ "https://t.me/polystat_org"] [text "Telegram group"]
            ]
        ],
      ul_
        []
        [ li_
            []
            [ a_
                [href_ "https://github.com/polystat/try-phi/stargazers"]
                [ img_
                    [ src_ "https://img.shields.io/github/stars/polystat/try-phi.svg?style=flat-square",
                      alt_ "github stars"
                    ]
                ]
            ]
        ]
    ]

editorDiv :: View action
editorDiv =
  div_
    [class_ "container-fluid", id_ "cont"]
    [ div_
        [class_ "col"]
        [ div_
            [class_ "row"]
            [ p_
                []
                [ text "Minimal ",
                  a_ [href_ "https://www.eolang.org"] [text "𝜑-calculus "],
                  text "expression (",
                  a_ [id_ "__permalink__", href_ "#"] [text "permalink"],
                  text ")",
                  infoIcon "info_editor"
                ],
              div_ [id_ "phi-editor"] []
            ]
        ]
    ]

eoLogoSection :: View action
eoLogoSection =
  section_
    []
    [ header_
        []
        [ center_
            []
            [ a_
                [href_ "https://www.eolang.org"]
                [ img_
                    [ src_ "https://www.yegor256.com/images/books/elegant-objects/cactus.png",
                      style_ $ Map.fromList [("width", "64px"), ("height", "64px")]
                    ]
                ]
            ]
        ]
    ]

cdns :: View action
cdns =
  div_
    []
    [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", rel_ "stylesheet", type_ "text/css"],
      script_ [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", type_ "text/javascript"] "",
      script_ [src_ "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-minimal-editor@v1.0/docs/build/bundle.js", type_ "text/javascript"] "",
      -- script_ [src_ "./editor/docs/build/bundle.js", type_ "text/javascript"] "",
      link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.3.0/font/bootstrap-icons.css", rel_ "stylesheet", type_ "text/css"],
      link_ [href_ "https://www.yegor256.com/images/books/elegant-objects/cactus.png", rel_ "shortcut icon"],
      link_ [href_ "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", rel_ "stylesheet", type_ "text/css"],
      link_ [href_ "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi/src/styles/styles.css", rel_ "stylesheet", type_ "text/css"],
      script_ [src_ "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi/src/scripts/init-popovers.js", type_ "text/javascript"] "",
      script_ [src_ "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi/src/scripts/set-snippet.js", type_ "text/javascript"] ""
    ]

getGraphSteps :: Model -> [CGraph.Configuration Gr]
getGraphSteps Model {..} =
  case modelAST of
    Left _ -> []
    Right term ->
      CGraph.steps @Gr (CGraph.initConfiguration term)

termTabs :: Phi.Term -> Model -> View Action
termTabs term m@Model {..} =
  div_
    []
    [ nav_
        []
        [ div_
            [class_ "nav nav-tabs", id_ "nav-tab", textProp "role" "tablist"]
            [ tabButton "button_eo" "content_eo" "info_eo" " EO code" Active,
              tabButton "button_original_term" "content_original_term" "info_original_term" " Original term" Disabled,
              tabButton "button_whnf" "content_whnf" "info_whnf" " Weak head normal form (WHNF)" Disabled,
              tabButton "button_nf" "content_nf" "info_nf" " Normal form (NF)" Disabled,
              tabButton "button_cbn_reduction" "content_cbn_reduction" "info_cbn_reduction" " Call-by-name term reduction" Disabled,
              tabButton "button_cbn_with_tap" "content_cbn_with_tap" "info_cbn_with_tap" " Call-by-name term reduction (via abstract machine)" Disabled,
              tabButton "button_cbn_with_graph" "content_cbn_with_graph" "info_cbn_with_graph" " Call-by-name evaluation on a graph" Disabled
            ]
        ],
      div_
        [class_ "tab-content", id_ "nav-tabContent"]
        [ tabContent "content_eo" (pre_ [] [text (ms (show $ ppTerm term))]) "button_eo" Active,
          tabContent "content_original_term" (pre_ [] [text (ms (show term))]) "button_original_term" Disabled,
          tabContent "content_whnf" (pre_ [] [text (ms (show (Phi.whnf term)))]) "button_whnf" Disabled,
          tabContent "content_nf" (pre_ [] [text (ms (show (Phi.nf term)))]) "button_nf" Disabled,
          -- cbn == Call by Name
          tabContent "content_cbn_reduction" (pre_ [] [text . ms . show $ Phi.ppWhnfSteps term]) "button_cbn_reduction" Disabled,
          tabContent "content_cbn_with_tap" (pre_ [] [text . ms . show $ Phi.ppStepsFor term]) "button_cbn_with_tap" Disabled,
          tabContent "content_cbn_with_graph" (graphContent term) "button_cbn_with_graph" Disabled
        ]
    ]
  where
    graphContent t =
      div_
        [class_ "row"]
        [ div_
            [class_ "col"]
            [ div_
                [class_ "row"]
                [ div_
                    [class_ "col"]
                    [ button_ [onClick PrevStep] [text "Previous step"],
                      button_ [onClick NextStep] [text "Next step"]
                    ],
                  div_ [class_ "col-2"] []
                ],
              pre_ [] [text . ms . show $ Phi.ppGraphStepsFor t graphStepNumber]
            ],
          div_
            [class_ "col"]
            [ img_
                [ let dotStringState = unpack $ CDot.renderAsDot @Gr (getGraphSteps m !! graphStepNumber)
                   in src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> dotStringState)),
                  height_ "400"
                ]
            ]
        ]

#ifndef __GHCJS__
codemirrorGetValue :: Model -> JSM MisoString
codemirrorGetValue Model{..} = do
  res <- eval $ textToStr (pack jsGetCode)
  str <- valToStr res
  let misoStr = ms $ strToText str
  return misoStr

#else
foreign import javascript unsafe "$r = app.view.state.doc.toString();"
  codemirrorGetValue :: IO MisoString
#endif
