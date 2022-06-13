module Phi
  ( Action(..)
  , State(..)
  , ParseError(..)
  , Term(..)
  , dataProp
  , eoLogoSection
  , html
  , Tab(..),
  TabId(..),
  Editor(..)
  )
  where

import Data.Eq

import CSS.Geometry as CG
import CSS.Size as CS
import Control.Applicative ((<$>))
import Data.Foldable (intercalate)
import Data.Int as DI
import Data.Map.Internal (Map)
import Data.Map.Internal as Map
import Data.Maybe (fromMaybe)
import Data.MediaType.Common (applicationJavascript, textCSS)
import Data.Tuple (Tuple(..))
import Halogen.HTML (AttrName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements (a, div, div_, i, img, li_, link, nav_, p_, script, ul_) as HE
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (ButtonType(..), IProp)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Prelude (class Show, discard, map, show, ($), (<>))
import Utils as U

data Editor = EOEditor | PhiEditor

-- TODO 
-- for recompilation, we will determine the source of code somehow

data Action
  = Recompile String
  | NextStep
  | PrevStep
  | SelectTab Tab
  | SelectCurrentEditor Editor

-- TODO
-- current editor is where the last change occured or the last cursor was left

html ∷ ∀ a. State -> HTML a Action
html model =
  HH.div
    [ HP.id "root" ]
    [
      cdns,
      eoLogoSection,
      editorDiv,
      HE.div [
        HP.id "app_div"
      ][
        let
          showError e = HH.div [U.class_ "pb-2"] [HH.pre_ [HH.text e]]
        in
          case model of
              ParseError e -> showError (show e)
              md -> HH.div_ [termTabs md]
      ],
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

dataBsProp ∷ ∀ a b. String → String → IProp a b
dataBsProp propName value = HP.attr (AttrName ("data-bs-" <> propName)) value


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
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L129"
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
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L155"
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
          , href: "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L98"
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
    , U.classes_ ["bi", "bi-info-square", "ms-2"]
    , dataBsProp "container" "body"
    , dataBsProp "toggle" "popover"
    , dataBsProp "placement" "top"
    , dataBsProp "content" (getInfoContent infoId)
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
                  HH.text "expression (",
                  HE.a [HP.id "__permalink__", HP.href "#"] [HH.text "permalink"],
                  HH.text ")",
                  infoIcon "info_editor"
                ],
              HH.div [U.class_ "mb-4", HP.id "phi-editor"] []
            ]
        ]
    ]


-- pageFooter :: View action
pageFooter ∷ ∀ a b. HTML a b
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


cdns ∷ ∀ a b. HTML a b
cdns =  
  HE.div_
    [ HE.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.script [HP.src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", HP.type_ applicationJavascript] [],
      -- TODO unsert into a separate tab
      -- TODO add tab switching with ctrl+tab
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@v1.0/docs/build/bundle.js", HP.type_ applicationJavascript] [],
      -- script_ [HP.src "./editor/docs/build/bundle.js", HP.type_ "text/javascript"] "",
      HE.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.0/font/bootstrap-icons.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.link [HP.href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", HP.rel "shortcut icon"],
      HE.link [HP.href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/init-popovers.js", HP.type_ applicationJavascript] [],
      HE.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/set-snippet.js", HP.type_ applicationJavascript] []
    ]



data Term = Term String

instance Show Term where
  show (Term s) = s


-- TODO EO and Phi tabs are synced
-- If there current tab is x in {Phi, EO}, and there's an error, model error will be about this tab
-- Otherwise, if code in current tab x is without errors, its contents will be translated into another tab

data ParseError = EOParseError String | PhiParseError String

instance Show ParseError where
  show (EOParseError s) = s
  show (PhiParseError s) = s

data State =
    ParseError ParseError 
  | State {
      currentEditor :: Editor,
      tabs :: Array Tab,
      graphStep :: Int
    }

data TabMode = Active | Disabled


tabButton :: forall a. Tab -> HTML a Action
tabButton t@(Tab tab) =
  HH.button [
    U.classes_ $ ["nav-link"] <> active,
    HP.id $ mkButton id,
    dataBsProp "toggle" "tab",
    dataBsProp "target" ("#" <> mkContent id),
    HP.type_ ButtonButton,
    HA.role "tab",
    U.attr_ "aria-controls" (mkContent id),
    U.attr_ "aria-selected" selected,
    HE.onClick $ \_ -> SelectTab t
  ] [
    HC.text (" " <> tab.buttonText),
    infoIcon (mkInfo id)
  ]
  where
    id = getId tab.id
    {t1: active, t2: selected} =
      if tab.isActive
      then {t1 : ["active"], t2: "true"}
      else {t1 : [], t2: "false"}


tabContent :: forall a b. Tab -> HTML a b
tabContent (Tab tab) =
  HH.div [
    U.classes_ $ ["tab-pane", "fade"] <> active,
    U.class_ "pt-3",
    HP.id id,
    U.attr_ "role" "tabpanel",
    U.attr_ "aria-labelledby" (mkButton id)
    ] [
    tab.tabContent
  ]
  where
  id = getId tab.id
  active =
    if tab.isActive
    then ["show", "active"]
    else []

mkButton ∷ String → String
mkButton id = "button_" <> id
mkContent ∷ String → String
mkContent id = "content_" <> id
mkInfo ∷ String → String
mkInfo id = "info_" <> id


data TabId = TEO | TTerm | TWHNF | TNF | TCBNReduction | TCBNWithTAP | TCBNWithGraph

derive instance Eq TabId

class IdGettable where
  getId :: TabId -> String

instance IdGettable where
  getId TEO = "eo"
  getId TTerm = "original-term"
  getId TWHNF = "whnf"
  getId TNF = "nf"
  getId TCBNReduction = "cbn-reduction"
  getId TCBNWithTAP = "cbn-with-tap"
  getId TCBNWithGraph = "cbn-with-graph"

data Tab = Tab {
  id :: TabId,
  buttonText :: String,
  isActive :: Boolean,
  tabContent :: forall a b. HTML a b
}

instance Eq Tab where
  eq (Tab t1) (Tab t2) = t1.id == t2.id

termTabs :: forall a. State -> HTML a Action
termTabs (ParseError s) = HH.div_ [HH.text (show s)]
termTabs (State {tabs : tabs'}) = 
  HH.div_ [
    HE.nav_ [
      HH.div
        [U.classes_ ["nav", "nav-tabs"], HP.id "nav-tab", HA.role "tablist"]
        (tabButton <$> tabs')
    ],
    HH.div 
      [U.class_ "tab-content", HP.id "nav-tabContent"]
      (tabContent <$> tabs')
  ]