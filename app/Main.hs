{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String

import qualified Phi.Minimal                      as Phi

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
  , modelAST    :: Phi.Term
  } deriving (Show, Eq)

-- | Sum type for application events
data Action
  = Recompile MisoString
  | Reload
  | NoOp
  deriving (Show, Eq)

initModel :: Model
initModel = Model
  { modelSource = ""
  , modelAST = Phi.ex6
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
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Reload m = m <# do
  Recompile <$> codemirrorGetValue
updateModel (Recompile _code) m = noEff m
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Model{..} = div_ []
  [ button_ [ onClick Reload ] [ text "Reload" ]
  , br_ []
  , br_ []
  , table_ [] [ tr_ []
    [ td_ []
        [ div_ [] [text "Original term:"]
        , pre_ [] [text (ms (show modelAST))] ]
    , td_ [ width_ "50" ] [ ]
    , td_ []
        [ div_ [] [text "Weak head normal form (WHNF):"]
        , pre_ [] [text (ms (show (Phi.whnf modelAST)))] ]
    , td_ [ width_ "50" ] [ ]
    , td_ []
        [ div_ [] [text "Normal form (NF):"]
        , pre_ [] [text (ms (show (Phi.nf modelAST)))] ]
    ] ]
  , br_ []

  , table_ [] [ tr_ []
    [ td_ []
        [ div_ [] [text "Call-by-name term reduction:"]
        , pre_ [] [text . ms . show $ Phi.ppWhnfSteps modelAST] ]
    , td_ [ width_ "50" ] [ ]
    , td_ []
        [ div_ [] [text "Call-by-name term reduction (via abstract machine):"]
        , pre_ [] [text . ms . show $ Phi.ppStepsFor modelAST] ]
    ] ]
  , br_ []

  , table_ [] [ tr_ []
    [ td_ []
        [ div_ [] [text "Call-by-name evaluation on a graph:"]
        , pre_ [] [text . ms . show $ Phi.ppGraphStepsFor modelAST]
        ]
    , td_ [ width_ "50" ] [ ]
    , td_ []
        [ img_ [ src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> Phi.renderAsDot modelAST))
               , height_ "400" ] ]
    ] ]
  , br_ []
  , pre_ [] [ text (ms (Phi.renderAsDot modelAST)) ]
  ]

#ifndef __GHCJS__
codemirrorGetValue :: JSM MisoString
codemirrorGetValue = return "[]"
#else
foreign import javascript unsafe "$r = myCodeMirror.getValue();"
  codemirrorGetValue :: IO MisoString
#endif
