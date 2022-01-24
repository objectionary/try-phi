{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Content where

import qualified Data.Map    as Map
import           Miso
import           Miso.String

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
    data_ "bs-target" ("#" <> contentId),
    type_ "button",
    textProp "role" "tab",
    textProp "aria-controls" contentId,
    textProp "aria-selected" selected
    ] [
    infoIcon infoId,
    text txt
  ]
  where
    (active, selected) =
      case isActive of
        Active -> (" active", "true")
        _      -> ("", "false")


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
        _      -> ""

infoContent :: MisoString -> MisoString
infoContent infoId = Map.findWithDefault "" infoId contents
  where
    contents :: Map.Map MisoString MisoString
    contents =
      Map.fromList
        [ ( "info_editor"
          , "<div>" <>
            "&bull; Original <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E'>syntax</a>" <>
            "<br>" <>
            "&bull; 𝜑-calculus <a href='https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASlupb0I'>definition</a>" <>
            "<br>" <>
            "&bull; <a href='https://bit.ly/32zuO4u'>BNF</a>" <> "<div>")
        , ( "info_editor"
          , "<div>" <>
            "&bull; Original <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E'>syntax</a>" <>
            "<br>" <>
            "&bull; 𝜑-calculus <a href='https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASlupb0I'>definition</a>" <>
            "<br>" <>
            "&bull; <a href='https://bit.ly/32zuO4u'>BNF</a>" <> "<div>")
        , ( "info_original_term"
          , "<div>" <>
            "&bull; Just prettyprint locators and brackets" <> "<div>")
        , ( "info_whnf"
          , "<div>" <>
            "&bull; <a href='https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L129'>Code</a>" <>
            "<br>" <>
            "&bull; Results from <a href='https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASJgTzO0'>Head reduction</a>" <>
            "<br>" <>
            "&bull; <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOc'>Relation to TAP machine configuration</a>" <>
            "<div>")
        , ( "info_nf"
          , "<div>" <>
            "&bull; <a href='https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L155'>Code</a>" <>
            "<br>" <>
            "&bull; Results from <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO4'>Normal order reduction</a>" <>
            "<div>")
        , ( "info_cbn_reduction"
          , "<div>" <>
            "&bull; <a href='https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L98'>Code</a>" <>
            "<br>" <>
            "&bull; Shows steps of <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO0'>Head reduction</a>" <>
            "<div>")
        , ( "info_cbn_with_tap"
          , "<div>" <>
            "&bull; <a href='https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName.hs#L85'>Code</a>" <>
            "<br>" <>
            "&bull; TAP machine <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOk'>definition</a>" <>
            "<br>" <>
            "&bull; TAP machine <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOo'>rules</a>" <>
            "<div>")
        , ( "info_cbn_with_graph"
          , "<div>" <>
            "&bull; <a href='https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName/Graph.hs#L77'>Code</a>" <>
            "<br>" <>
            "&bull; It does apply <a href='https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOw'>Decorated Instantiation</a>" <>
            "<div>")
        , ( "info_eo"
          , "<div>" <>
            "φ-term translated to EO" <>
            "<div>")
        ]
