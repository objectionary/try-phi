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
  , modelAST    :: Either String Phi.Term
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
  , modelAST = Left "initializing..."
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
updateModel (Recompile code) m = noEff m
  { modelAST = Phi.parseTerm (fromMisoString code) }
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Model{..} =
  case modelAST of
    Left err -> div_ []
      [ button_ [ onClick Reload ] [ text "Reload" ]
      , br_ []
      , pre_ [] [ text (ms err) ]
      ]
    Right term -> div_ []
      [ button_ [ onClick Reload ] [ text "Reload" ]
      , br_ []
      , br_ []
      , table_ [] [ tr_ []
        [ td_ []
            [ div_ [] [text "Original term:"]
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
      , br_ []

      , table_ [] [ tr_ []
        [ td_ []
            [ div_ [] [text "Call-by-name term reduction:"]
            , pre_ [] [text . ms . show $ Phi.ppWhnfSteps term] ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ div_ [] [text "Call-by-name term reduction (via abstract machine):"]
            , pre_ [] [text . ms . show $ Phi.ppStepsFor term] ]
        ] ]
      , br_ []

      , table_ [] [ tr_ []
        [ td_ []
            [ div_ [] [text "Call-by-name evaluation on a graph:"]
            , pre_ [] [text . ms . show $ Phi.ppGraphStepsFor term]
            ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ img_ [ src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> Phi.renderAsDot term))
                   , height_ "400" ] ]
        , td_ [ width_ "50" ] [ ]
        , td_ []
            [ img_ [ src_ (ms ("https://quickchart.io/graphviz?layout=dot&format=svg&graph=" <> Phi.renderAsColorfulDot term))
                   , height_ "400" ] ]
        ] ]
      , br_ []
      , pre_ [] [ text (ms (Phi.renderAsDot term)) ]
      , br_ []
      , pre_ [] [ text (ms (Phi.renderAsColorfulDot term)) ]
      ]

#ifndef __GHCJS__
codemirrorGetValue :: JSM MisoString
codemirrorGetValue = return (ms (show Phi.ex6))
#else
foreign import javascript unsafe "$r = myCodeMirror.getValue();"
  codemirrorGetValue :: IO MisoString
#endif
