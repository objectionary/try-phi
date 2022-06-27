module Phi
  ( Action(..)
  , Editor(..)
  , ParseError(..)
  , State(..)
  , Tab
  , TabId(..)
  , Term(..)
  , component
  , dataProp
  , editorId
  , eoLogoSection
  , html
  , md1
  , OkState(..)
  )
  where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AW
import CSS.Geometry as CG
import CSS.Size as CS
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Argonaut.Decode (JsonDecodeError) as AD
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Array as DA
import Data.Either (Either(..), hush)
import Data.Foldable (intercalate, traverse_)
import Data.Int as DI
import Data.Map (fromFoldable)
import Data.Map.Internal (Map)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJavascript, textCSS)
import Data.String.Common (null)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Console (log)
import Foreign (unsafeFromForeign)
import Halogen (AttrName(..), Component)
import Halogen as H
import Halogen.HTML (HTML, b)
import Halogen.HTML (button, header_, pre_, section_, span, span_, text) as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements (a, div, div_, i, img, li_, link, nav_, p_, script, ul_, h3_) as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events (onClick) as HH
import Halogen.HTML.Properties (ButtonType(..), IProp)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (role) as HA
import Halogen.Query.Event (eventListener)
import Utils (attr_, class_, classes_) as U
import Utils (clipboard, makePermalink, writeText)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (getAttribute, setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.CustomEvent as CE
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location (search) as Web
import Web.HTML.Window (document, location) as Web
import Web.HTML.Window (navigator)
import Web.URL.URLSearchParams as USP
import WebSender (handleAction)

data Editor = EOEditor | PhiEditor

derive instance Eq Editor

ids ∷ Array TabId
ids = [ TEO, TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP, TCBNWithGraph ]

defaultOk :: OkState
defaultOk = {
  graphStep : 1,
  tabs:
        ( \( Tuple
              ( Tuple a b
            )
              c
          ) ->
            Tab { id: a, buttonText: b, isActive: c, tabContent: HH.text "" }
        )
          <$> DA.zip (DA.zip ids btexts) isActives
    }
  where
  
  btexts = [ "EO code", "Phi term", " WHNF", "NF", "CBN Reduction", "CBN With TAP", "CBN With Graph" ]

  isActives = [ true, false, false, false, false, false, false ]


md1 :: State
md1 =
    { currentEditor: EOEditor
    , info: Left NoCode
    }
  
-- TODO no tabs when empty editors
-- Prompt to type in one of the editors
-- md2 :: State
-- md2 = 

data Request = Request { code :: String}

instance EncodeJson Request where
  encodeJson (Request r) = 
      "code" := r.code
      ~> jsonEmptyObject

urlPrefix ∷ String
-- urlPrefix = "http://localhost:3000/"
-- urlPrefix = "http://localhost:8082/"
urlPrefix = "https://try-phi-back.herokuapp.com/"

-- FIXME
-- add error entry

data Response = Response {
  code :: String,
  tabs :: Map TabId String
}

instance DecodeJson Response where
  decodeJson json = do
    x <- decodeJson json
    code <- x .: "code"
    tabs <- x .: "tabs"
    ps <- traverse (\y -> (\z -> Tuple y z) <$> tabs .: (getName y)) ids
    let ps' = fromFoldable ps
    pure $ Response {code: code, tabs: ps'}

myHref_ ∷ String
myHref_ = "my-href"
editor_ ∷ String
editor_ = "editor"
permalink_ ∷ String
permalink_ = "permalink"
snippet_ ∷ String
snippet_ = "snippet"

log_ :: forall output. String -> H.HalogenM State Action () output Aff Unit
log_ = H.liftEffect <<< log

class LogError a where
  err :: forall output. a -> H.HalogenM State Action () output Aff Unit

data HandleError = 
    NoSnippetOrEditor 
  | NoPermalink
  | NoResponse
  | NoHrefPermalink

instance LogError HandleError where
  err NoSnippetOrEditor = log_ "check 'snippet' or 'editor'"
  err NoPermalink = log_ "no permalink"
  err NoResponse = log_ "no response received!"
  err NoHrefPermalink = log_ "no href in permalink!"

component :: ∀ a b c. Component a b c Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState _ = md1

  render s = html s

  handleAction :: forall output. Action -> H.HalogenM State Action () output Aff Unit
  handleAction = case _ of
    Init -> do
      handleAction $ UpdatePermalink EOEditor ""
      handleAction ListenEditorsCodeChanged
      handleAction ListenEditorsCreated
    SelectTab t1 -> do
      let selectActiveTab = (\x -> x { tabs = ( \t2@(Tab t') -> Tab (t' { isActive = t1 == t2 })) <$> x.tabs})
      updateInfo selectActiveTab
            
    NextStep -> updateInfo $ \x -> x {graphStep = x.graphStep + 1}
    PrevStep -> updateInfo $ \x -> x {graphStep = x.graphStep - 1}
    ListenEditorsCodeChanged -> forEditors ListenEditorCodeChanged
    ListenEditorCodeChanged e -> do
        document <- H.liftEffect $ Web.document =<< Web.window
        H.subscribe' \_ ->
          eventListener
          (EventType (getNameEventCodeChanged e))
          (HTMLDocument.toEventTarget document)
          (\ev -> do
            let
              editor = getEditorWhereCodeChanged $ (\(EventType s) -> s) $ Event.type_ ev
              code' :: Maybe (Either AD.JsonDecodeError NewCode)
              code' = CE.fromEvent ev <#> CE.detail <#> unsafeFromForeign <#> decodeJson
              code = 
                case code' of
                  Just (Right c) -> Just c.newCode
                  _ -> Nothing
            HandleEditorCodeChanged <$> editor <*> code)

    HandleEditorCodeChanged editor code -> do
      if null code
      then handleAction $ HandleError NoCode
      -- FIXME only set when there was an error in state
      else H.modify_ $ \s -> 
        case s.info of 
          Left _ -> s {info = Right defaultOk}
          Right _ -> s

      setEditor editor
      handleAction $ UpdatePermalink editor code
      -- request code from server
      let req = Request {
          code: code
        }
      resp <- H.liftAff $ AX.put AW.driver AXRF.json (urlPrefix <> editorName editor) (Just $ Json $ encodeJson req)
      let
        -- FIXME handle errors
        resp' :: Maybe Response
        resp' = 
            case resp of
              Right {body: b} -> hush $ decodeJson b
              Left _ -> Nothing
      case resp' of
        Just (Response r) -> 
          do
            setCode (anotherEditor editor) r.code
            updateInfo $ \s -> s {
              tabs = (\(Tab t) -> 
                case Map.lookup t.id r.tabs of
                  Just cont -> Tab t {tabContent = HH.pre_ [HH.text cont]}
                  Nothing -> Tab t {tabContent = HH.pre_ [HH.text "No response"]}
                ) <$> s.tabs 
              }

        -- FIXME Handle error
        Nothing -> err NoResponse

    CopyToClipboard -> do
      cb <- H.liftEffect $ Web.window >>= navigator >>= clipboard
      -- get link
      perm <- H.liftEffect $ Web.window >>= Web.document >>= (\x -> getElementById permalink_ $ toNonElementParentNode (toDocument x))
      case perm of
        Just j -> do
          h <- H.liftEffect $ getAttribute myHref_ j
          case h of
            Just h' -> H.liftEffect $ pure h' >>= writeText cb
            Nothing -> err NoHrefPermalink
        Nothing -> err NoPermalink

    UpdatePermalink editor code -> do
      perm <- H.liftEffect $ Web.window >>= Web.document >>= (\x -> getElementById permalink_ $ toNonElementParentNode (toDocument x))
      case perm of
        Just p -> do
          ref <- H.liftEffect $ makePermalink (editorName editor) code
          H.liftEffect $ setAttribute myHref_ ref p
        Nothing -> err NoPermalink

    -- FIXME react to an event from editor when it is created to send the code
    -- if it sends earlier => it's present, so send the code (init from link) even if no event received
    -- editors notify when created
    ListenEditorsCreated -> forEditors ListenEditorCreated
    ListenEditorCreated editor -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \_ ->
        eventListener
        (EventType (getNameEventEditorCreated editor))
        (HTMLDocument.toEventTarget document)
        (\_ -> Just $ InitFromLink editor)
    InitEditorsFromLink -> forEditors InitFromLink
    InitFromLink editor -> do
      -- take data for setting initial snippet
      link <- USP.fromString <$> (H.liftEffect $ Web.window >>= Web.location >>= Web.search)
      let
        ed = USP.get editor_ link >>= editorId
        snip = USP.get snippet_ link
      case snip /\ ed of
        Just snippet' /\ Just editor' -> do
          setCode editor' snippet'
          -- handle code change when it is caused by user actions
          when (editor == editor') (handleAction (HandleEditorCodeChanged editor' snippet'))
        _ -> err NoSnippetOrEditor
    HandleError error -> H.modify_ $ \s -> s {info = Left error}
    NoOp -> pure unit

  setEditor :: forall output. Editor -> H.HalogenM State Action () output Aff Unit
  setEditor editor = H.modify_ $ \s -> s {currentEditor = editor}

  updateInfo :: forall output. (OkState -> OkState) -> H.HalogenM State Action () output Aff Unit
  updateInfo f = H.modify_ $ \s -> s {info = f <$> s.info}

  forEditors :: forall output. (Editor -> Action) -> H.HalogenM State Action () output Aff Unit
  forEditors f = traverse_ handleAction (map (\e -> f e) [EOEditor, PhiEditor])
  
  setCode :: forall output. Editor -> String -> H.HalogenM State Action () output Aff Unit
  setCode editor code = do
      ce <- H.liftEffect $ CE.toEvent <$> CE.new' (EventType $ (getNameEventChangeCode editor)) (Just {newCode: code})
      doc <- H.liftEffect $ toEventTarget <$> (Web.document =<< Web.window)
      H.liftEffect $ dispatchEvent ce doc *> pure unit

-- FIXME remove non-polymorphic classes

-- instance CodeChanges where
anotherEditor :: Editor -> Editor
anotherEditor EOEditor = PhiEditor
anotherEditor PhiEditor = EOEditor

editorName :: Editor -> String
editorName EOEditor = "eo"
editorName PhiEditor = "phi"  

codeChangedSuff :: String
codeChangedSuff = "-editor-code-changed"

getNameEventCodeChanged :: Editor -> String
getNameEventCodeChanged x = editorName x <> codeChangedSuff

getEditorWhereCodeChanged :: String -> Maybe Editor
getEditorWhereCodeChanged s
  | s == getNameEventCodeChanged EOEditor = Just EOEditor
  | s == getNameEventCodeChanged PhiEditor = Just PhiEditor
  | otherwise = Nothing

changeCodeSuff :: String
changeCodeSuff = "-editor-change-code"

getNameEventChangeCode :: Editor -> String
getNameEventChangeCode x = editorName x <> changeCodeSuff

editorId :: String -> Maybe Editor
editorId s
  | s == "eo" = Just EOEditor
  | s == "phi" = Just PhiEditor
  | otherwise = Nothing

editorCreatedSuff ∷ String
editorCreatedSuff = "-editor-created"

getNameEventEditorCreated :: Editor -> String
getNameEventEditorCreated e = editorName e <> editorCreatedSuff

data Action
  = Init
  | NextStep
  | PrevStep
  | SelectTab Tab
  | ListenEditorsCodeChanged
  | ListenEditorCodeChanged Editor
  | HandleEditorCodeChanged Editor String
  | CopyToClipboard
  | UpdatePermalink Editor String
  | InitFromLink Editor
  | NoOp
  | ListenEditorsCreated
  | ListenEditorCreated Editor
  | InitEditorsFromLink
  | HandleError ParseError
type NewCode = {
  newCode :: String
}

-- TODO
-- current editor is 
-- for now: where the last change occured
-- ideally: the last cursor was left

justifyContentCenter ∷ String
justifyContentCenter = "justify-content-center"

dFlex ∷ String
dFlex = "d-flex"

dGrid = "d-grid"

html ∷ ∀ a. State -> HTML a Action
html state =
  HH.div
    [ U.classes_ [dGrid, "gap-1"] ] $
        divRow <$> [
          cdns,
          eoLogoSection,
          permalinkButton,
          editorDiv,
          infos state,
          pageFooter
        ]
  where
  divRow x = HH.div [U.classes_ ["p-1"]] [ x ]

permalinkButton :: forall a. HTML a Action
permalinkButton = 
  HH.div [U.classes_ [dFlex, justifyContentCenter]] [
    HH.button [HP.type_ ButtonButton, U.classes_ ["btn", "btn-warning"], HP.id permalink_, onClick $ \_ -> CopyToClipboard] [HH.text $ "Copy permalink"]
  ]
          

infos :: ∀ a. State -> HTML a Action
infos s = case s.info of
  Left e -> showError e
  Right r -> termTabs r
  where
    pleaseCheck s e = "Please, check your " <> s <> " code:\n" <> e
    message (EOParseError e) = pleaseCheck "EO" e
    message (PhiParseError e) = pleaseCheck "Phi" e
    message NoCode = "Type something!"
    showError e = HH.div [U.classes_ ["text-center"]] [HH.h3_ [HH.span [HP.style "font-weight:normal"] [HH.text $ message e]]]


eoLogoSection ∷ ∀ a b. HTML a b
eoLogoSection =
  HH.section_
    [ HH.header_
        [ 
          HH.div [U.classes_ [dFlex, justifyContentCenter]][
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
  , { x: getName TTerm
    , y:
        [ { pref: "Just prettyprint locators and brackets"
          , href: "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
          , txt: "syntax"
          }
        ]
    }
  , { x: getName TWHNF
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
  , { x: getName TNF
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
  , { x: getName TCBNReduction
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
  , { x: getName TCBNWithTAP
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
  , { x: getName TCBNWithGraph
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
    -- TODO function for editors
        [U.class_ "row"] [
          HH.div [U.classes_ ["col-sm-6"]] [
            HH.div [U.class_ "row"] [
              HH.div [U.classes_ [dFlex, justifyContentCenter]] [
                HH.p_
                [ HH.a [HP.href "https://arxiv.org/abs/2204.07454"] [HH.text "𝜑-calculus "],
                  HH.text "expression",
                  -- FIXME edit popover
                  infoIcon "info_phi_editor"
                ]
              ]
            ],
            HH.div [U.classes_ [dFlex, justifyContentCenter]] [
              HH.div [U.class_ "row"] [
                HH.div [HP.id "phi-editor"] []
              ]
            ]
          ],
          HH.div [U.classes_ ["col-sm-6"]] [
            HH.div [U.class_ "row"] [
              HH.div [U.classes_ [dFlex, justifyContentCenter]] [
                HH.p_
                [ HH.a [HP.href "https://www.eolang.org"] [HH.text "EO "],
                  HH.text "expression",
                  -- FIXME edit popover
                  infoIcon "info_eo_editor"
                ]
              ]
            ],
            HH.div [U.class_ "row"] [
              HH.div [U.classes_ [dFlex, justifyContentCenter]] [
                HH.div [HP.id "eo-editor"] []
              ]
            ]
          ]
        ]
    ]

-- pageFooter :: View action
pageFooter ∷ ∀ a b. HTML a b
pageFooter =
  HH.nav_
    [ HH.ul_
        [ HH.li_
            [ HH.text "Join our ",
              HH.a [HP.href "https://t.me/polystat_org"] [HH.text "Telegram group"],
              HH.text " to discuss how 𝜑-calculus works"
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
      -- TODO insert into a separate tab
      -- TODO add tab switching with ctrl+tab
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d58d13becead90f7aaf11e424df8663532e85a23/docs/eo-editor.js", U.attr_ "type" "module"] [],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d58d13becead90f7aaf11e424df8663532e85a23/docs/eo-editor.css", HP.type_ textCSS],
      
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@c04746d8040a6fcad2efd94b14014defeaccacf4/docs/phi-editor.js", U.attr_ "type" "module"] [],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@c04746d8040a6fcad2efd94b14014defeaccacf4/docs/phi-editor.css", HP.type_ textCSS],
      
      HH.link [HP.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.0/font/bootstrap-icons.css", HP.rel "stylesheet", HP.type_ textCSS],
      HH.link [HP.href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", HP.rel "shortcut icon"],
      HH.link [HP.href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", HP.rel "stylesheet", HP.type_ textCSS],
      HH.script [HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/try-phi@0.0.1/src/Site/scripts/init-popovers.js", HP.type_ applicationJavascript] []
    ]

data Term = Term String

instance Show Term where
  show (Term s) = s


-- | TODO EO and Phi tabs are synced
-- | Phi code is translated into EO code (and vice-versa) on a server
-- | Currently, the state will store just the parse errors, including
-- TODO provide error message
data ParseError = EOParseError String | PhiParseError String | NoCode


instance Show ParseError where
  show (EOParseError s) = s
  show (PhiParseError s) = s
  show NoCode = "no code"

type OkState = {
  tabs :: Array Tab,
  graphStep :: Int
}

-- FIXME store the last editor even in case of parse errors
-- then no need to store a flag about non-empty code, can use just an error
type State = {
  currentEditor :: Editor,
  info :: Either ParseError OkState
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
    id = getName tab.id
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
  id = getName tab.id
  active =
    if tab.isActive
    then ["show", "active"]
    else []

mkButton ∷ String → String
mkButton id = "button_" <> id
mkContent ∷ String → String
mkContent id = "content_" <> id
mkInfo ∷ String → String
mkInfo id = "info" <> id

data TabId = TEO | TTerm | TWHNF | TNF | TCBNReduction | TCBNWithTAP | TCBNWithGraph

derive instance Eq TabId
derive instance Ord TabId
-- instance GenericShow TabId where
-- derive instance GenericShow TabId

class IdGettable a where
  getName :: a -> String
  getId :: String -> Maybe a

instance IdGettable TabId where
  getName TEO = "eo"
  getName TTerm = "original_term"
  getName TWHNF = "whnf"
  getName TNF = "nf"
  getName TCBNReduction = "cbn_reduction"
  getName TCBNWithTAP = "cbn_with_tap"
  getName TCBNWithGraph = "cbn_with_graph"
  getId x
    | x == "eo" = Just TEO
    | x == "original_term" = Just TTerm
    | x == "whnf" = Just TWHNF
    | x == "nf" = Just TNF
    | x == "cbn_reduction" = Just TCBNReduction
    | x == "cbn_with_tap" = Just TCBNWithTAP
    | x == "cbn_with_graph" = Just TCBNWithGraph
    | otherwise = Nothing

data Tab = Tab {
  id :: TabId,
  buttonText :: String,
  isActive :: Boolean,
  tabContent :: forall a b. HTML a b
}

instance Eq Tab where
  eq (Tab t1) (Tab t2) = t1.id == t2.id

termTabs :: forall a. OkState -> HTML a Action
termTabs {tabs : tabs'} = 
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