module Phi
  ( component
  , globalVarDev
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AW
import CSS.Geometry as CG
import CSS.Size as CS
import Data.Argonaut (encodeJson)
import Data.Argonaut.Decode (JsonDecodeError) as AD
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Array (length, (!!), (:))
import Data.Array as DA
import Data.Either (Either(..), hush)
import Data.Foldable (intercalate, traverse_)
import Data.Int as DI
import Data.Map.Internal (Map)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType.Common (applicationJavascript, textCSS)
import Data.String.Common (null)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Foreign (unsafeFromForeign)
import Halogen (AttrName(..), Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML (button, header_, pre, pre_, section_, span, text) as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements (a, div, div_, i, img, li_, link, nav_, p_, script, ul_, h3_) as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events (onClick) as HH
import Halogen.HTML.Properties (ButtonType(..), IProp, style)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (role) as HA
import Halogen.Query.Event (eventListener)
import PhiDefinitions (Action(..), AppState(..), Editor(..), ErrorState, GraphTab(..), HandleError(..), NewCode, OkState, ParseError(..), Request(..), Response(..), State, Tab(..), TabId(..), err, getName, log_)
import Utils (attr_, class_, classes_) as U
import Utils (clipboard, createPopovers, makePermalink, readGlobalBoolean, removePopovers, writeText)
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

overTabs :: OkState -> (Tab -> Tab) -> OkState
overTabs st f = st { textTabs = f <$> st.textTabs, graphTab = f st.graphTab }

-- / Component /
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
  handleAction ac = do
    do
      case ac of
        Init -> do
          handleAction $ UpdatePermalink EOEditor ""
          handleAction ListenEditorsCodeChanged
          handleAction ListenEditorsCreated
          setStatePopovers
        -- FIXME include graph tab
        SelectTab t1 -> updateInfo $ \x -> overTabs x (\t2@(Tab t) -> Tab (t { isActive = t1 == t2 }))
        NextStep -> updateInfo $ \x -> x { graphStep = min (x.graphStep + 1) ((length ((\(GraphTab g) -> g.graphs) $ x.graphTabState)) - 1) }
        PrevStep -> updateInfo $ \x -> x { graphStep = max (x.graphStep - 1) 0 }
        ListenEditorsCodeChanged -> forEditors ListenEditorCodeChanged
        ListenEditorCodeChanged e -> do
          document <- H.liftEffect $ Web.document =<< Web.window
          H.subscribe' \_ ->
            eventListener
              (EventType (getNameEventCodeChanged e))
              (HTMLDocument.toEventTarget document)
              ( \ev -> do
                  let
                    editor = getEditorWhereCodeChanged $ (\(EventType s) -> s) $ Event.type_ ev

                    code' :: Maybe (Either AD.JsonDecodeError NewCode)
                    code' = CE.fromEvent ev <#> CE.detail <#> unsafeFromForeign <#> decodeJson

                    code = case code' of
                      Just (Right c) -> Just c.newCode
                      _ -> Nothing
                  HandleEditorCodeChanged <$> editor <*> code
              )
        HandleEditorCodeChanged editor code -> do
          -- FIXME only set when there was an error in state
          del <- deleteStatePopovers
          when del
            $ do
                -- do
                if null code then
                  handleAction $ HandleError NoCode
                else
                  H.modify_
                    $ \s -> case s.info of
                        Left _ -> s { info = Right defaultOk }
                        Right _ -> s
                setEditor editor
                handleAction $ UpdatePermalink editor code
                -- request code from server
                let
                  req =
                    Request
                      { code: code
                      }
                isDev <- H.liftEffect $ readGlobalBoolean globalVarDev
                let
                  appState = case isDev of
                    Just true -> DevState
                    _ -> DeployState
                resp <- H.liftAff $ AX.put AW.driver AXRF.json (urlPrefix appState <> editorName editor) (Just $ Json $ encodeJson req)
                let
                  -- FIXME handle errors
                  resp' :: Maybe Response
                  resp' = case resp of
                    Right { body: b } -> hush $ decodeJson b
                    Left _ -> Nothing
                case resp' of
                  Just (OkResponse r) -> do
                    setCode (anotherEditor editor) r.code
                    updateInfo
                      $ \s ->
                          s
                            { textTabs =
                              ( \(Tab t) -> case Map.lookup t.id r.textTabs of
                                  Just cont -> Tab t { tabContent = HH.pre_ [ HH.text cont ] }
                                  Nothing -> Tab t { tabContent = HH.pre_ [ HH.text "No response" ] }
                              )
                                <$> s.textTabs
                            , graphTabState = r.graphTab
                            , graphStep = 0
                            }
                            # \s1 ->
                                s1
                                  { graphTab = (\(Tab t) -> Tab t { tabContent = getGraphTabContent s1 }) s1.graphTab
                                  }
                    -- st <- H.get
                    -- case st.info of
                    --   Right { graphStep } -> log_ $ show graphStep
                    --   Left _ -> pure unit
                    setStatePopovers
                  Just (ErrorResponse r) ->
                    handleAction $ HandleError
                      $ ( r.error
                            # case editor of
                                EOEditor -> EOParseError
                                PhiEditor -> PhiParseError
                        )
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
          -- set popover
          setEditorPopovers
          -- log_ $ show editorPopovers
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
        HandleError error -> do
          H.modify_ $ \s -> s { info = Left $ defaultError { parseError = error } }
          setStatePopovers
        NoOp -> pure unit

  forEditors :: forall output. (Editor -> Action) -> H.HalogenM State Action () output Aff Unit
  forEditors f = traverse_ handleAction (map (\e -> f e) [ EOEditor, PhiEditor ])

setEditor :: forall output. Editor -> H.HalogenM State Action () output Aff Unit
setEditor editor = H.modify_ $ \s -> s { currentEditor = editor }

updateInfo :: forall output. (OkState -> OkState) -> H.HalogenM State Action () output Aff Unit
updateInfo f = H.modify_ $ \s -> s { info = f <$> s.info }

setCode :: forall output. Editor -> String -> H.HalogenM State Action () output Aff Unit
setCode editor code = do
  ce <- H.liftEffect $ CE.toEvent <$> CE.new' (EventType $ (getNameEventChangeCode editor)) (Just { newCode: code })
  doc <- H.liftEffect $ toEventTarget <$> (Web.document =<< Web.window)
  H.liftEffect $ dispatchEvent ce doc *> pure unit

setStatePopovers :: forall output. H.HalogenM State Action () output Aff Unit
setStatePopovers = H.gets _.info >>= (\x -> H.liftEffect $ createPopovers $ getInfoIds x)

deleteStatePopovers :: forall output. H.HalogenM State Action () output Aff Boolean
deleteStatePopovers = H.gets _.info >>= (\x -> H.liftEffect $ removePopovers $ getInfoIds x)

setEditorPopovers :: forall output. H.HalogenM State Action () output Aff Unit
setEditorPopovers = H.liftEffect $ createPopovers editorPopovers

textStepButton :: Action -> String
textStepButton = case _ of
  PrevStep -> "Previous step"
  NextStep -> "Next step"
  _ -> ""

-- / Stringification /
urlPrefix ∷ AppState -> String
urlPrefix DevState = "http://localhost:8082/"

-- urlPrefix DevState = "https://try-phi-back.herokuapp.com/"
urlPrefix DeployState = "https://try-phi-back.herokuapp.com/"

_editor ∷ String → String
_editor x = x <> "_editor"

myHref_ ∷ String
myHref_ = "my-href"

editor_ ∷ String
editor_ = "editor"

permalink_ ∷ String
permalink_ = "permalink"

snippet_ ∷ String
snippet_ = "snippet"

globalVarDev :: String
globalVarDev = "dev"

editorPopovers ∷ Array String
editorPopovers = (\x -> mkInfo (editorName x) # _editor) <$> [ EOEditor, PhiEditor ]

editorName :: Editor -> String
editorName EOEditor = "eo"

editorName PhiEditor = "phi"

editorNamePretty :: Editor -> String
editorNamePretty EOEditor = "EO"

editorNamePretty PhiEditor = "𝜑-calculus"

codeChangedSuff :: String
codeChangedSuff = "-editor-code-changed"

getNameEventCodeChanged :: Editor -> String
getNameEventCodeChanged x = editorName x <> codeChangedSuff

changeCodeSuff :: String
changeCodeSuff = "-editor-change-code"

getNameEventChangeCode :: Editor -> String
getNameEventChangeCode x = editorName x <> changeCodeSuff

editorCreatedSuff ∷ String
editorCreatedSuff = "-editor-created"

getNameEventEditorCreated :: Editor -> String
getNameEventEditorCreated e = editorName e <> editorCreatedSuff

justifyContentCenter ∷ String
justifyContentCenter = "justify-content-center"

dFlex ∷ String
dFlex = "d-flex"

dGrid ∷ String
dGrid = "d-grid"

getInfoContent ∷ String → String
getInfoContent x = fromMaybe "" (Map.lookup x infoContent)

infoContent ∷ Map String String
infoContent = Map.fromFoldable $ map (\{ x, y } -> Tuple x (combine y)) ics
  where
  _a :: Maybe String -> Maybe String -> String
  _a Nothing Nothing = ""

  _a Nothing (Just s) = s

  _a (Just h) Nothing = h

  _a (Just h) (Just s) = "<a href=" <> h <> ">" <> s <> "</a>"

  maybeEmptyString :: Maybe String -> String
  maybeEmptyString = maybe "" identity

  _br :: String -> String
  _br s = "<br>" <> s

  _div :: String -> String
  _div s = "<div>" <> s <> "</div>"

  _bullet :: String -> String
  _bullet s = "&bull; " <> s

  combine y = _div $ intercalate "<br>" (map (\{ pref, href, txt } -> (_bullet $ (maybeEmptyString pref) <> " " <> _a href txt)) y)

errorPopover :: String
errorPopover = "error"

getInfoIds :: Either ErrorState OkState -> Array String
getInfoIds (Left st) = map (\(Tab t) -> mkInfo $ getName t.id) [ st.errorTab ]

getInfoIds (Right st) = map (\(Tab t) -> mkInfo $ getName t.id) st.textTabs

mkButton ∷ String → String
mkButton id = "button_" <> id

mkContent ∷ String → String
mkContent id = "content_" <> id

mkInfo ∷ String → String
mkInfo id = "info_" <> id

-- / Data /
ics ∷ Array { x ∷ String, y ∷ Array { href :: Maybe String, pref :: Maybe String, txt :: Maybe String } }
ics =
  (\r@{ x: x } -> r { x = mkInfo x })
    <$> [ { x: editorName PhiEditor # _editor
        , y:
            [ { pref: Just "Original"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
              , txt: Just "syntax"
              }
            , { pref: Just $ editorNamePretty PhiEditor
              , href: Just "https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASlupb0I"
              , txt: Just "definition"
              }
            , { pref: Nothing
              , href: Just "https://bit.ly/32zuO4u"
              , txt: Just "BNF"
              }
            ]
        }
      , { x: editorName EOEditor # _editor
        , y:
            [ { pref: Nothing
              , href: Just "https://arxiv.org/pdf/2111.13384.pdf#page=3"
              , txt: Just "BNF and syntax explanation"
              }
            ]
        }
      , { x: getName TTerm
        , y:
            [ { pref: Just "Just prettyprint locators and brackets"
              , href: Nothing
              , txt: Nothing
              }
            , { pref: Just "Original"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAATVEUf-E"
              , txt: Just "Syntax"
              }
            ]
        }
      , { x: getName TWHNF
        , y:
            [ { pref: Nothing
              , href: Just "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L129"
              , txt: Just "Code"
              }
            , { pref: Just "Results from"
              , href: Just "https://drive.google.com/file/d/1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH/edit?disco=AAAASJgTzO0"
              , txt: Just "Head reduction"
              }
            , { pref: Nothing
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOc"
              , txt: Just "Relation to TAP machine configuration"
              }
            ]
        }
      , { x: getName TNF
        , y:
            [ { pref: Nothing
              , href: Just "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L155"
              , txt: Just "Code"
              }
            , { pref: Just "Results from"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO4"
              , txt: Just "Normal order reduction"
              }
            ]
        }
      , { x: getName TCBNReduction
        , y:
            [ { pref: Just "Call by Name Reduction"
              , href: Nothing
              , txt: Nothing
              }
            , { pref: Nothing
              , href: Just "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/State.hs#L98"
              , txt: Just "Code"
              }
            , { pref: Just "Shows steps of"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzO0"
              , txt: Just "Head reduction"
              }
            ]
        }
      , { x: getName TCBNWithTAP
        , y:
            [ { pref: Nothing
              , href: Just "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName.hs#L85"
              , txt: Just "Code"
              }
            , { pref: Just "TAP machine"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOk"
              , txt: Just "definition"
              }
            , { pref: Just "TAP machine"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOo"
              , txt: Just "rules"
              }
            ]
        }
      , { x: getName TCBNWithGraph
        , y:
            [ { pref: Nothing
              , href: Just "https://github.com/br4ch1st0chr0n3/try-phi/blob/c738694f771c10ffa11f34fa23bf54220d2653c7/src/Phi/Minimal/Machine/CallByName/Graph.hs#L77"
              , txt: Just "Code"
              }
            , { pref: Just "It does apply"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOw"
              , txt: Just "Decorated Instantiation"
              }
            ]
        }
      , { x: errorPopover
        , y:
            [ { pref: Just "For now, we only report syntactic errors"
              , href: Nothing
              , txt: Nothing
              }
            ]
        }
      ]

-- / Constants /
textTabIds ∷ Array TabId
-- FIXME exclude cbnwithGraph
textTabIds = [ TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP ]

errorTabId :: TabId
errorTabId = TError

anotherEditor :: Editor -> Editor
anotherEditor EOEditor = PhiEditor

anotherEditor PhiEditor = EOEditor

getEditorWhereCodeChanged :: String -> Maybe Editor
getEditorWhereCodeChanged s
  | s == getNameEventCodeChanged EOEditor = Just EOEditor
  | s == getNameEventCodeChanged PhiEditor = Just PhiEditor
  | otherwise = Nothing

editorId :: String -> Maybe Editor
editorId s
  | s == "eo" = Just EOEditor
  | s == "phi" = Just PhiEditor
  | otherwise = Nothing

-- / State /
defaultOk :: OkState
defaultOk =
  { graphStep: 1
  , textTabs:
      ( \( Tuple
            ( Tuple a b
          )
            c
        ) ->
          Tab { id: a, buttonText: b, isActive: c, tabContent: HH.div_ [] }
      )
        <$> DA.zip (DA.zip textTabIds btexts) isActives
  , graphTab: Tab { id: TCBNWithGraph, buttonText: "CBN with Graph", isActive: false, tabContent: HH.div_ [] }
  , graphTabState:
      GraphTab
        { states: []
        , graphs: []
        }
  }
  where
  btexts = [ "Phi term", " WHNF", "NF", "CBN Reduction", "CBN With TAP", "CBN With Graph" ]

  isActives = [ true, false, false, false, false, false, false ]

defaultError :: ErrorState
defaultError =
  { errorTab: Tab { id: errorTabId, isActive: isActive, buttonText: bText, tabContent: HH.div_ [] }
  , parseError: NoCode
  }
  where
  bText = "Errors"

  isActive = true

md1 :: State
md1 =
  { currentEditor: EOEditor
  , info: Left defaultError
  }

-- / Logic /
setTab :: forall a. Tab -> ParseError -> Tab
setTab (Tab t) pe = t'
  where
  pleaseCheck t1 = "Please, check your " <> t1 <> " code:\n"

  getMessage = case _ of
    EOParseError e' -> e'
    PhiParseError e' -> e'
    NoCode -> "Type something!"

  getEditor e =
    pleaseCheck
      $ case e of
          EOParseError _ -> (editorNamePretty EOEditor)
          PhiParseError _ -> (editorNamePretty PhiEditor)
          NoCode -> "empty editors'"

  ppParseError :: forall b. ParseError -> HTML b Action
  ppParseError e = HH.div_ [ HH.pre [ style "white-space: pre-wrap" ] [ HH.text $ getMessage e ] ]

  ppErrorMessage :: forall b. ParseError -> HTML b Action
  ppErrorMessage e = HH.div [ U.classes_ [] ] [ HH.h3_ [ HH.span [ HP.style "font-weight:normal" ] [ HH.text $ getEditor e ] ] ]

  showError :: forall b. ParseError -> HTML b Action
  showError e = HH.div [ U.classes_ [ "d-flex" ] ] [ HH.div_ [ ppErrorMessage e, ppParseError e ] ]

  -- FIXME
  t' = Tab t { tabContent = showError pe }

-- / Props /
dataBsProp ∷ ∀ a b. String → String → IProp a b
dataBsProp propName value = HP.attr (AttrName ("data-bs-" <> propName)) value

-- / HTML /
html ∷ ∀ a. State -> HTML a Action
html state =
  HH.div
    [ U.classes_ [ dGrid, "gap-1" ] ]
    $ divRow
    <$> [ cdns
      , eoLogoSection
      , permalinkButton
      , editorsDiv
      , infos state
      , pageFooter
      ]
  where
  divRow x = HH.div [ U.classes_ [ "p-1" ] ] [ x ]

permalinkButton :: forall a. HTML a Action
permalinkButton =
  HH.div [ U.classes_ [ dFlex, justifyContentCenter ] ]
    [ HH.button
        [ HP.type_ ButtonButton
        , U.classes_
            [ "btn", "btn-warning"
            ]
        , HP.id permalink_
        , onClick $ \_ -> CopyToClipboard
        ]
        [ HH.text $ "Copy permalink" ]
    ]

infos :: ∀ a. State -> HTML a Action
infos s = case s.info of
  Left e -> errorTab e
  Right r -> termTabs r

errorTab :: forall a. ErrorState -> HTML a Action
errorTab { parseError: pe, errorTab: et } =
  -- TODO where to set tab content?
  HH.div_
    [ HH.nav_
        [ HH.div
            [ U.classes_ [ "nav", "nav-tabs" ], HP.id "nav-tab", HA.role "tablist" ]
            [ tabButton et' ]
        ]
    , HH.div
        [ U.class_ "tab-content", HP.id "nav-tabContent" ]
        [ tabContent et' ]
    ]
  where
  et' = setTab et pe

eoLogoSection ∷ ∀ a b. HTML a b
eoLogoSection =
  HH.section_
    [ HH.header_
        [ HH.div [ U.classes_ [ dFlex, justifyContentCenter ] ]
            [ HH.a [ HP.href "https://www.eolang.org" ]
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

infoIcon :: forall a b. String -> HTML a b
infoIcon infoId =
  HH.i
    [ HP.id infoId
    , U.classes_ [ "bi", "bi-info-square", "ms-2" ]
    , dataBsProp "container" "body"
    , dataBsProp "toggle" "popover"
    , dataBsProp "placement" "top"
    , dataBsProp "content" (getInfoContent infoId)
    ]
    []

editorDiv :: forall a b. Editor -> String -> HTML a b
editorDiv ed ref =
  HH.div [ U.classes_ [ "col-sm-6" ] ]
    [ HH.div [ U.class_ "row" ]
        [ HH.div [ U.classes_ [ dFlex, justifyContentCenter ] ]
            [ HH.p_
                [ HH.a [ HP.href ref ] [ HH.text $ editorNamePretty ed <> " " ]
                , HH.text "code"
                -- FIXME edit popover 
                , infoIcon $ mkInfo (editorName ed) # _editor
                ]
            ]
        ]
    , HH.div [ U.classes_ [ dFlex, justifyContentCenter ] ]
        [ HH.div [ U.class_ "row" ]
            [ HH.div [ HP.id $ editorName ed <> "-editor" ] []
            ]
        ]
    ]

editorsDiv :: forall a b. HTML a b
editorsDiv =
  HH.div
    [ U.class_ "container-fluid", HP.id "cont" ]
    [ HH.div
        [ U.class_ "row" ]
        [ editorDiv PhiEditor "https://arxiv.org/abs/2204.07454"
        , editorDiv EOEditor "https://www.eolang.org"
        ]
    ]

-- pageFooter :: View action
pageFooter ∷ ∀ a b. HTML a b
pageFooter =
  HH.nav_
    [ HH.ul_
        [ HH.li_
            [ HH.text "Join our "
            , HH.a [ HP.href "https://t.me/polystat_org" ] [ HH.text "Telegram group" ]
            , HH.text " to discuss how 𝜑-calculus works"
            ]
        ]
    , HH.ul_
        [ HH.li_
            [ HH.a
                [ HP.href "https://github.com/polystat/try-phi/stargazers" ]
                [ HH.img
                    [ HP.src "https://img.shields.io/github/stars/polystat/try-phi.svg?style=flat-square"
                    , HP.alt "github stars"
                    ]
                ]
            ]
        ]
    ]

cdns ∷ ∀ a b. HTML a b
cdns =
  HH.div_
    [ HH.link [ HP.href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css", HP.rel "stylesheet", HP.type_ textCSS ]
    , HH.script [ HP.src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", HP.type_ applicationJavascript ] []
    -- TODO insert into a separate tab 
    -- TODO add tab switching with ctrl+tab
    , HH.script [ HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d58d13becead90f7aaf11e424df8663532e85a23/docs/eo-editor.js", U.attr_ "type" "module" ] []
    , HH.link [ HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@d58d13becead90f7aaf11e424df8663532e85a23/docs/eo-editor.css", HP.type_ textCSS ]
    , HH.script [ HP.src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@c04746d8040a6fcad2efd94b14014defeaccacf4/docs/phi-editor.js", U.attr_ "type" "module" ] []
    , HH.link [ HP.href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@c04746d8040a6fcad2efd94b14014defeaccacf4/docs/phi-editor.css", HP.type_ textCSS ]
    , HH.link [ HP.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.0/font/bootstrap-icons.css", HP.rel "stylesheet", HP.type_ textCSS ]
    , HH.link [ HP.href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", HP.rel "shortcut icon" ]
    , HH.link [ HP.href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", HP.rel "stylesheet", HP.type_ textCSS ]
    ]

tabButton :: forall a. Tab -> HTML a Action
tabButton t@(Tab tab) =
  HH.button
    [ U.classes_ $ [ "nav-link" ] <> active
    , HP.id $ mkButton id
    , dataBsProp "toggle" "tab"
    , dataBsProp "target" ("#" <> mkContent id)
    , HP.type_ ButtonButton
    , HA.role "tab"
    , U.attr_ "aria-controls" (mkContent id)
    , U.attr_ "aria-selected" selected
    , HH.onClick $ \_ -> SelectTab t
    ]
    [ HC.text (" " <> tab.buttonText)
    , infoIcon (mkInfo id)
    ]
  where
  id = getName tab.id

  { t1: active, t2: selected } =
    if tab.isActive then
      { t1: [ "active" ], t2: "true" }
    else
      { t1: [], t2: "false" }

tabContent :: forall a. Tab -> HTML a Action
tabContent (Tab tab) =
  HH.div
    [ U.classes_ $ [ "tab-pane", "fade" ] <> active
    , U.class_ "pt-3"
    , HP.id id
    , U.attr_ "role" "tabpanel"
    , U.attr_ "aria-labelledby" (mkButton id)
    ]
    [ tab.tabContent
    ]
  where
  id = getName tab.id

  active =
    if tab.isActive then
      [ "show", "active" ]
    else
      []

allTabs :: OkState -> Array Tab
allTabs st = st.textTabs <> [ st.graphTab ]

termTabs :: forall a. OkState -> HTML a Action
termTabs st =
  HH.div_
    [ HH.nav_
        [ HH.div
            [ U.classes_ [ "nav", "nav-tabs" ], HP.id "nav-tab", HA.role "tablist" ]
            (tabButton <$> allTabs st)
        ]
    , HH.div
        [ U.class_ "tab-content", HP.id "nav-tabContent" ]
        (tabContent <$> st.textTabs <> [(\(Tab t') -> Tab t' {tabContent = getGraphTabContent st}) st.graphTab])
    ]

stepButton :: forall b. Action -> HTML b Action
stepButton a =
  HH.button
    [ HP.type_ ButtonButton
    , U.classes_ [ "btn", "btn-warning" ]
    , onClick $ \_ -> a
    ]
    [ HH.text $ textStepButton a ]

getGraphTabContent :: forall b. OkState -> HTML b Action
getGraphTabContent { graphTabState: GraphTab { states, graphs }, graphStep } =
  HH.div [ U.class_ "row" ]
    [ HH.div [ U.class_ "col-sm" ]
        [ stepButton PrevStep, stepButton NextStep
        , HH.pre_ [ HH.text $ maybe ("step error: " <> show graphStep) identity (states !! graphStep) ]
        , HH.text $ show graphStep
        ]
    , HH.div [ U.class_ "col-sm" ]
        [ HH.img
            [ HP.src $ maybe ("step error: " <> show graphStep) ((<>) "https://quickchart.io/graphviz?layout=dot&format=svg&graph=") (graphs !! graphStep)
            ]
        ]
    ]
