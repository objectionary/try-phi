module Phi
  ( Model(..)
  , dataProp
  , eoLogoSection
  , html
  , Term(..)
  )
  where

import Data.Map.Internal
import Prelude

import CSS.Geometry as CG
import CSS.Size as CS
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Int as DI
import Data.List (foldl)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJavascript, textCSS)
import Data.Tuple (Tuple(..))
import Halogen.HTML (AttrName(..), ClassName(..), HTML(..), PropName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core (attr)
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements as HE
import Halogen.HTML.Elements.Keyed (div_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), IProp, href)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Utils as U
import Web.HTML.Common as WC

data Action
  = Recompile String
  | Reload
  | NoOp
  | NextStep
  | PrevStep

html ∷ ∀ a b. Model -> HTML a b
html md =
  HH.div
    [ HP.id "root" ]
    [
      cdns,
      eoLogoSection,
      editorDiv,
      HE.div [
        HP.id "app_div"
      ][
        HE.button [onClick (\_ -> Reload), U.class_ "btn btn-secondary mb-5"] [HH.text "Reload"],
        let {t : term, d: divElem} = 
                case md.modelAST of
                  Left err -> {t: Term "Error", d: HH.div [U.class_ "pb-2"] [HH.pre_ [HH.text err]]}
                  Right t' -> {t: t', d: HH.div_ []}
        in HH.div_ [divElem, termTabs term md {modelAst : Right term}]
      ]
      pageFooter
    ]

eoLogoSection ∷ ∀ a b. HTML a b
eoLogoSection =
  HH.section_
    [ HH.header_
        [ -- TODO center
          HH.a [ HP.href "https://www.eolang.org" ]
            [ HE.img
                [ HP.src "https://www.yegor256.com/images/books/elegant-objects/cactus.png"
                , CSS.style do
                    CG.width (CS.px $ DI.toNumber 64)
                    CG.height (CS.px $ DI.toNumber 64)
                ]
            ]
        ]
    ]

dataProp ∷ ∀ a b. String → String → IProp a b
dataProp propName value = HP.attr (AttrName ("data-" <> propName)) value

getInfoContent ∷ String → String
getInfoContent x = fromMaybe "" (Map.lookup x infoContent)

infoContent ∷ Map String String
infoContent = Map.fromFoldable $ map (\{ x, y } -> Tuple x (combine y)) ics
  where
  _a :: String -> String -> String
  _a "" "" = ""
  _a "" s = s
  _a h "" = h
  _a h s = "<a href=" <> h <> ">" <> s <> "</a>"

  _br :: String -> String
  _br s = "<br>" <> s

  _div :: String -> String
  _div s = "<div>" <> s <> "</div>"

  _bullet :: String -> String
  _bullet s = "&bull;" <> s

  combine y = _div $ intercalate "<br>" (map (\{ pref, href, txt } -> (_bullet $ pref <> " " <> _a href txt)) y)


ics ∷ Array { x ∷ String , y ∷ Array { href :: String , pref :: String , txt :: String } }
ics =
  [ { x: "info_editor"
    , y:
        [ { pref: "Original"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
          , txt: "syntax"
          }
        , { pref: "𝜑-calculus"
          , href: "https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASlupb0I"
          , txt: "definition"
          }
        , { pref: ""
          , href: "https://bit.ly/32zuO4u"
          , txt: "BNF"
          }
        ]
    }
  , { x: "info_original_term"
    , y:
        [ { pref: "Just prettyprint locators and brackets"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
          , txt: "syntax"
          }
        ]
    }
  , { x: "info_whnf"
    , y:
        [ { pref: ""
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L129"
          , txt: "Code"
          }
        , { pref: "Results from"
          , href: "https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASJgTzO0"
          , txt: "Head reduction"
          } 
        , { pref: ""
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOc"
          , txt: "Relation to TAP machine configuration"
          }
        ]
    }
  , { x: "info_nf"
    , y:
        [ { pref: ""
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L155"
          , txt: "Code"
          }
        , { pref: "Results from"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO4"
          , txt: "Normal order reduction"
          }
        ]
    }
  , { x: "info_cbn_reduction"
    , y:
        [ { pref: ""
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Model.hs#L98"
          , txt: "Code"
          }
        , { pref: "Shows steps of"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO0"
          , txt: "Head reduction"
          }
        ]
    }
  , { x: "info_cbn_with_tap"
    , y:
        [ { pref: ""
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName.hs#L85"
          , txt: "Code"
          }
        , { pref: "TAP machine"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOk"
          , txt: "definition"
          }
        , { pref: "TAP machine"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOo"
          , txt: "rules"
          }
        ]
    }
  , { x: "info_cbn_with_graph"
    , y:
        [ { pref: ""
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName/Graph.hs#L77"
          , txt: "Code"
          }
        , { pref: "It does apply"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOw"
          , txt: "Decorated Instantiation"
          }
        ]
    }
  , { x: "info_eo"
    , y:
        [ { pref: "𝜑-term translated to EO"
          , href: ""
          , txt: ""
          }
        ]
    }
  ]

infoIcon :: forall a b. String -> HTML a b
infoIcon infoId =
  HE.i
    [ HP.id infoId
    , U.class_ "bi bi-info-square"
    , dataProp "bs-container" "body"
    , dataProp "bs-toggle" "popover"
    , dataProp "bs-placement" "top"
    , dataProp "bs-content" (getInfoContent infoId)
    ]
    []

editorDiv :: forall a b. HTML a b
editorDiv =
  HH.div
    [U.class_ "container-fluid", HP.id "cont"]
    [ HH.div
        [U.class_ "col"]
        [ HH.div
            [U.class_ "row"]
            [ HE.p_
                [ HH.text "Minimal ",
                  HE.a [HP.href "https://www.eolang.org"] [HH.text "𝜑-calculus "],
                  -- a_ [href_ "https://www.eolang.org"] [text "𝜑-calculus "],
                  HH.text "expression (",
                  HE.a [HP.id "__permalink__", HP.href "#"] [HH.text "permalink"],
                  -- a_ [id_ "__permalink__", href_ "#"] [text "permalink"],
                  HH.text ")",
                  infoIcon "info_editor"
                ],
              HH.div [HP.id "phi-editor"] []
            ]
        ]
    ]



data TabMode = Active | Disabled

tabButton :: forall a b. String -> String -> String -> String -> TabMode -> HTML a b
tabButton buttonId contentId infoId txt isActive =
  HH.button [
    U.class_ $ "nav-link" <> active,
    HP.id buttonId,
    dataProp "bs-toggle" "tab",
    dataProp "bs-target" ("#" <> contentId),
    HP.type_ ButtonButton,
    HA.role "tab",
    U.attr_ "aria-controls" contentId,
    U.attr_ "aria-selected" selected
    ] [
    infoIcon infoId,
    HC.text txt
  ]
  where
    {t1: active, t2: selected} =
      case isActive of
        Active -> {t1 : " active", t2: "true"}
        _      -> {t1 : "", t2: "false"}

tabContent :: forall a b. String -> HTML a b-> String -> TabMode -> HTML a b
tabContent tabId content buttonId isActive =
  HH.div [
    U.class_ $ "tab-pane fade" <> active,
    U.class_ "pt-3",
    HP.id tabId,
    U.attr_ "role" "tabpanel",
    U.attr_ "aria-labelledby" buttonId
    ] [
    content
  ]
  where
    active =
      case isActive of
        Active -> " show active"
        _      -> ""


-- pageFooter :: View action
pageFooter =
  HE.nav_
    [ HE.ul_
        [ HE.li_
            [ HH.text "To discuss how 𝜑-calculus works, join our ",
              HE.a [HP.href "https://t.me/polystat_org"] [HH.text "Telegram group"]
            ]
        ],
      HE.ul_
        [ HE.li_
            [ HE.a
                [HP.href "https://github.com/polystat/try-phi/stargazers"]
                [ HE.img
                    [ HP.src "https://img.shields.io/github/stars/polystat/try-phi.svg?style=flat-square",
                      HP.alt "github stars"
                    ]
                ]
            ]
        ]
    ]


cdns =
  HE.div_
    [ HE.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.script [HP.src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", HP.type_ applicationJavascript] [],
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-minimal-editor@v1.0/docs/build/bundle.js", HP.type_ applicationJavascript] [],
      -- script_ [HP.src "./editor/docs/build/bundle.js", HP.type_ "text/javascript"] "",
      HE.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.3.0/font/bootstrap-icons.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.link [HP.href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", HP.rel "shortcut icon"],
      HE.link [HP.href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.link [HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi/src/styles/styles.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/init-popovers.js", HP.type_ applicationJavascript] [],
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/set-snippet.js", HP.type_ applicationJavascript] []
    ]

data Term = Term String

instance Show Term where
  show (Term s) = s

term = "Term"

data Model = Model
  { modelSource      :: String,
    modelAST         :: Either String Term,
    graphStepNumber  :: Int,
    jsGetCode        :: String,
    popoversScript   :: String,
    setSnippetScript :: String
  }

termTabs :: forall a. Term -> Model -> HTML a Action
termTabs term m =
  HH.div_
    [ 
      HE.nav_
        [ HH.div
            [U.class_ "nav nav-tabs", HP.id "nav-tab", U.attr_ "role" "tablist"]
            [ tabButton "button_eo" "content_eo" "info_eo" " EO code" Active,
              tabButton "button_original_term" "content_original_term" "info_original_term" " Original term" Disabled,
              tabButton "button_whnf" "content_whnf" "info_whnf" " Weak head normal form (WHNF)" Disabled,
              tabButton "button_nf" "content_nf" "info_nf" " Normal form (NF)" Disabled,
              tabButton "button_cbn_reduction" "content_cbn_reduction" "info_cbn_reduction" " Call-by-name term reduction" Disabled,
              tabButton "button_cbn_with_tap" "content_cbn_with_tap" "info_cbn_with_tap" " Call-by-name term reduction (via abstract machine)" Disabled,
              tabButton "button_cbn_with_graph" "content_cbn_with_graph" "info_cbn_with_graph" " Call-by-name evaluation on a graph" Disabled
            ]
        ],
      HH.div
        [U.class_ "tab-content", HP.id "nav-tabContent"]
        [ tabContent "content_eo" (HE.pre_ [HH.text (show term)]) "button_eo" Active,
          tabContent "content_original_term" (HE.pre_ [HH.text (show term)]) "button_original_term" Disabled,
          tabContent "content_whnf" (HE.pre_ [HH.text (show term)]) "button_whnf" Disabled,
          tabContent "content_nf" (HE.pre_ [HH.text (show term)]) "button_nf" Disabled,
          -- cbn == Call by Name
          tabContent "content_cbn_reduction" (HE.pre_ [HH.text (show term)]) "button_cbn_reduction" Disabled,
          tabContent "content_cbn_with_tap" (HE.pre_ [HH.text (show term)]) "button_cbn_with_tap" Disabled,
          tabContent "content_cbn_with_graph" (graphContent term) "button_cbn_with_graph" Disabled
        ]
    ]
  where
    graphContent t =
      HH.div
        [U.class_ "row"]
        [ HH.div
            [U.class_ "col"]
            [ HH.div
                [U.class_ "row"]
                [ HH.div
                    [U.class_ "col"]
                    [ HH.button [onClick (\_ -> PrevStep)] [HH.text "Previous step"],
                      HH.button [onClick (\_ -> NextStep)] [HH.text "Next step"]
                    ],
                  HH.div [U.class_ "col-2"] []
                ],
              HH.pre_ [
                  HH.text (show term)
                ]
            ]
        ]