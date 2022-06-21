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
  State'(..),
  class CodeChanges,
  getCodeChangedEventName,
  getChangeCodeEventName,
  getEditorWhereCodeChanged,
  codeChangedSuff,
  changeCodeSuff,
  editorName,
  anotherEditor,
   md1,
   component,
   Editor(..)
  )
  where

import Foreign
import Prelude
import Web.Event.EventTarget
import Web.HTML.HTMLDocument

import Affjax.RequestBody (document)
import CSS.Geometry as CG
import CSS.Size as CS
import Data.Argonaut.Decode (JsonDecodeError) as AD
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable (intercalate, traverse_)
import Data.Int as DI
import Data.Map.Internal (Map)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJavascript, textCSS)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (AttrName(..), Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML (button, header_, pre_, section_, text) as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements (a, div, div_, i, img, li_, link, nav_, p_, script, ul_) as HH
import Halogen.HTML.Events (handler, onClick) as HH
import Halogen.HTML.Properties (ButtonType(..), IProp)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (role) as HA
import Halogen.Query.Event (eventListener)
import Utils (classes_, class_, attr_) as U
import Web.Event.CustomEvent as CE
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.XHR.XMLHttpRequest (open')

data Editor = EOEditor | PhiEditor

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
            Tab { id: a, buttonText: b, isActive: c, tabContent: HH.text b }
        )
          <$> DA.zip (DA.zip ids btexts) isActives
    -- FIXME store code of editors?
    -- send immediately?
    }
  where
  ids = [ TEO, TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP, TCBNWithGraph ]

  btexts = [ "EO code", "Phi term", " WHNF", "NF", "CBN Reduction", "CBN With TAP", "CBN With Graph" ]

  isActives = [ true, false, false, false, false, false, false ]

initTab :: Tab
initTab = Tab { id: TEO, buttonText: "EO code", isActive: true, tabContent: HH.text "EO code" }

data Request
  = Request
    { editor :: Editor,
      code :: String
    }

-- FIXME make a request to a server
getState :: Request -> State
getState s = md1

component :: ∀ a b c. Component a b c Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just ListenToEditors
              -- (SelectTab initTab)
              }
    }
  where
  initialState _ = md1

  render s = html s

  handleAction :: forall output. Action -> H.HalogenM State Action () output Aff Unit
  handleAction = case _ of
    SelectTab t1 ->
      H.modify_
        $ (<$>) \s ->
            ( s
                { tabs =
                  ( \t2@(Tab t') ->
                      Tab (t' { isActive = t1 == t2 })
                  )
                    <$> s.tabs
                }
            )
    NextStep -> H.modify_ $ (<$>) \s -> s { graphStep = s.graphStep + 1 }
    PrevStep -> H.modify_ $ (<$>) \s -> s { graphStep = s.graphStep - 1 }
    SelectCurrentEditor e -> H.modify_ $ (<$>) \s -> s { currentEditor = e }
    Recompile c -> H.modify_ $ \_ -> getState (Request { code: c, editor: EOEditor })
    ListenToEditors -> traverse_ handleAction (map (\e -> ListenToEditor e) [EOEditor, PhiEditor])
    ListenToEditor e -> do
        document <- H.liftEffect $ Web.document =<< Web.window
        H.subscribe' \_ ->
          eventListener
          (EventType (getCodeChangedEventName e))
          (HTMLDocument.toEventTarget document)
          (\ev -> do
            let
              editor = getEditorWhereCodeChanged $ (\(EventType s) -> s) $ Event.type_ ev
              -- code = ""
              code' :: Maybe (Either AD.JsonDecodeError NewCode)
              code' = CE.fromEvent ev <#> CE.detail <#> unsafeFromForeign <#> decodeJson
              code = 
                case code' of
                  Just (Right c) -> Just c.newCode
                  _ -> Nothing
              op = HandleEditorCodeChanged <$> editor <*> code
            case op of
              Nothing -> pure NoOp
              Just op' -> pure op')
    
    -- FIXME use that code
    HandleEditorCodeChanged editor code -> do
      ce <- H.liftEffect $ CE.toEvent <$> CE.new' (EventType $ getChangeCodeEventName (anotherEditor editor)) (Just {newCode: code})
      doc <- H.liftEffect $ toEventTarget <$> (Web.document =<< Web.window)
      _ <- H.liftEffect $ dispatchEvent ce doc
      H.modify_ $ (<$>) \s -> s {currentEditor = editor, graphStep = s.graphStep + 2}
    
    -- FIXME a more appropriate function
    NoOp -> H.modify_ identity

class ChangeCodeEvent where
  getCodeChangeEventName :: Editor -> String

class CodeChanges where
  getCodeChangedEventName :: Editor -> String
  getEditorWhereCodeChanged :: String -> Maybe Editor
  getChangeCodeEventName :: Editor -> String
  codeChangedSuff :: String
  changeCodeSuff :: String
  editorName :: Editor -> String
  anotherEditor :: Editor -> Editor

instance CodeChanges where
  anotherEditor EOEditor = PhiEditor
  anotherEditor PhiEditor = EOEditor
  editorName EOEditor = "eo"
  editorName PhiEditor = "phi"
  codeChangedSuff = "-editor-code-changed"
  getCodeChangedEventName x = editorName x <> codeChangedSuff
  -- getCodeChangedEventName PhiEditor = "phi" <> codeChangedSuff
  getEditorWhereCodeChanged s
    | s == getCodeChangedEventName EOEditor = Just EOEditor
    | s == getCodeChangedEventName PhiEditor = Just PhiEditor
    | otherwise = Nothing
  changeCodeSuff = "-editor-change-code"
  getChangeCodeEventName x = editorName x <> changeCodeSuff



data Action
  = Recompile String
  | NextStep
  | PrevStep
  | SelectTab Tab
  | SelectCurrentEditor Editor
  | ListenToEditors
  | ListenToEditor Editor
  | HandleEditorCodeChanged Editor String
  | NoOp

type NewCode = {
  newCode :: String
}

-- TODO
-- current editor is 
-- for now: where the last change occured
-- ideally: the last cursor was left

html ∷ ∀ a. State -> HTML a Action
html state =
  HH.div
    [ HP.id "root" ]
    [
      cdns,
      eoLogoSection,
      editorDiv,
      HH.div [
        HP.id "app_div"
      ][
        let
          showError e = HH.div [U.class_ "pb-2"] [HH.pre_ [HH.text e]]
        in
          case state of
              ParseError e -> showError (show e)
              md -> termTabs md
      ],
      HH.div_ 
      [
        HH.text $ case state of 
          ParseError e -> show e
          -- FIXME this is a temporary output just to check if event handling
          State s -> (show s.graphStep)
      ],
      pageFooter
    ]

eoLogoSection ∷ ∀ a b. HTML a b
eoLogoSection =
  HH.section_
    [ HH.header_
        [ -- TODO center
          HH.a [ HP.href "https://www.eolang.org" ]
            [ HH.img
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
  _bullet s = "&bull; " <> s

  combine y = _div $ intercalate "<br>" (map (\{ pref, href, txt } -> (_bullet $ pref <> " " <> _a href txt)) y)


ics ∷ Array { x ∷ String , y ∷ Array { href :: String , pref :: String , txt :: String } }
ics = (\r@{x: x} -> r {x = mkInfo x}) <$>
  [ { x: "editor"
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
  , { x: getId TTerm
    , y:
        [ { pref: "Just prettyprint locators and brackets"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
          , txt: "syntax"
          }
        ]
    }
  , { x: getId TWHNF
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
  , { x: getId TNF
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
  , { x: getId TCBNReduction
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
  , { x: getId TCBNWithTAP
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
  , { x: getId TCBNWithGraph
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
  , { x: "eo"
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
  HH.i
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
            [ HH.p_
                [ HH.text "Minimal ",
                  HH.a [HP.href "https://www.eolang.org"] [HH.text "𝜑-calculus "],
                  HH.text "expression (",
                  HH.a [HP.id "__permalink__", HP.href "#"] [HH.text "permalink"],
                  HH.text ")",
                  infoIcon "info_editor"
                ],
              HH.div [U.classes_ ["mb-4", "col-sm"], HP.id "phi-editor"] [],
              HH.div [U.classes_ ["mb-4", "col-sm"], HP.id "eo-editor"] []
            ]
        ]
    ]


-- pageFooter :: View action
pageFooter ∷ ∀ a b. HTML a b
pageFooter =
  HH.nav_
    [ HH.ul_
        [ HH.li_
            [ HH.text "To discuss how 𝜑-calculus works, join our ",
              HH.a [HP.href "https://t.me/polystat_org"] [HH.text "Telegram group"]
            ]
        ],
      HH.ul_
        [ HH.li_
            [ HH.a
                [HP.href "https://github.com/polystat/try-phi/stargazers"]
                [ HH.img
                    [ HP.src "https://img.shields.io/github/stars/polystat/try-phi.svg?style=flat-square",
                      HP.alt "github stars"
                    ]
                ]
            ]
        ]
    ]


cdns ∷ ∀ a b. HTML a b
cdns =  
  HH.div_
    [ HH.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HH.script [HP.src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", HP.type_ applicationJavascript] [],
      -- TODO unsert into a separate tab
      -- TODO add tab switching with ctrl+tab
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d1b8c1a912f780f7e96e78f6bc71eaa35c78186d/docs/eo-editor.js", U.attr_ "type" "module"] [],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d1b8c1a912f780f7e96e78f6bc71eaa35c78186d/docs/eo-editor.css", HP.type_ textCSS],
      
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@8bd224677c5e03adc4609589d948f8b6b0ee2456/docs/phi-editor.js", U.attr_ "type" "module"] [],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@8bd224677c5e03adc4609589d948f8b6b0ee2456/docs/phi-editor.css", HP.type_ textCSS],
      
      HH.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.0/font/bootstrap-icons.css", HP.rel "stylesheet", HP.type_ textCSS],
      HH.link [HP.href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", HP.rel "shortcut icon"],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/init-popovers.js", HP.type_ applicationJavascript] [],
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/set-snippet.js", HP.type_ applicationJavascript] []
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

data State' a b = ParseError a | State b

instance Functor (State' a) where
  map f (State s) = State (f s)
  map _ (ParseError e) = (ParseError e)

type State = State'
  ParseError 

  {
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
    HH.onClick $ \_ -> SelectTab t
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

class IdGettable a where
  getId :: a -> String

instance IdGettable TabId where
  getId TEO = "eo"
  getId TTerm = "original_term"
  getId TWHNF = "whnf"
  getId TNF = "nf"
  getId TCBNReduction = "cbn_reduction"
  getId TCBNWithTAP = "cbn_with_tap"
  getId TCBNWithGraph = "cbn_with_graph"

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
    HH.nav_ [
      HH.div
        [U.classes_ ["nav", "nav-tabs"], HP.id "nav-tab", HA.role "tablist"]
        (tabButton <$> tabs')
    ],
    HH.div 
      [U.class_ "tab-content", HP.id "nav-tabContent"]
      (tabContent <$> tabs')
  ]

-- TODO handler on `document`
-- put a handler on it
-- https://pursuit.purescript.org/packages/purescript-halogen/6.1.2/docs/Halogen.HTML.Events#v:handler
-- convert Event to CustomEvent
-- https://pursuit.purescript.org/packages/purescript-web-events/4.0.0/docs/Web.Event.CustomEvent#v:fromEvent
-- take the detail info
-- https://pursuit.purescript.org/packages/purescript-web-events/4.0.0/docs/Web.Event.CustomEvent#v:detail
-- read newCode
-- https://pursuit.purescript.org/packages/purescript-foreign/7.0.0/docs/Foreign.Index#v:readProp
-- convert to a string
-- https://github.com/purescript/purescript-foreign/blob/v7.0.0/src/Foreign.purs#L138-L138

-- or create a custom function to decode Foreign
-- https://book.purescript.org/chapter10.html#json
-- and check for validity, create a record from it