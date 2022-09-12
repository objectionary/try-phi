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
import Data.Array (findIndex, length, (!!))
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
import Halogen (AttrName(..), Component, get)
import Halogen as H
import Halogen.HTML (HTML, button, header_, pre, pre_, section_, span, text)
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements (a, div, div_, i, img, li_, link, nav_, p_, script, ul_, h3_, nav, h5)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), IProp, style, id, type_, attr, href, src, alt, rel)
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
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.URL.URLSearchParams as USP

overTabs :: OkState -> (Tab -> Tab) -> OkState
overTabs st f = st { textTabs = f <$> st.textTabs }

-- #Component
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

-- TODO 

  handleAction :: forall output. Action -> H.HalogenM State Action () output Aff Unit
  handleAction ac = do
    do
      case ac of
        Init -> do
          handleAction $ UpdatePermalink EOEditor ""
          handleAction ListenEditorsCodeChanged
          handleAction ListenEditorsCreated
          setStatePopovers
          doc <- H.liftEffect $ Web.document =<< Web.window
          H.subscribe' \sid ->
            eventListener
              KET.keyup
              (HTMLDocument.toEventTarget doc)
              (map (HandleKey sid) <<< KE.fromEvent)
          setCurrentTab
        SelectTab id -> do
          -- set current tab
          updateInfo $ \x -> overTabs x (\(Tab t) -> Tab (t { isActive = id == t.id }))
          -- update state
          H.modify_ $ \x -> x { currentTab = id }
        -- TODO update graph content
        UpdateGraphContent ->
          updateInfo
            $ \x ->
                overTabs x
                  ( \t1@(Tab t) -> case t.id of
                      TCBNWithGraph -> Tab (t { tabContent = getGraphTabContent x.graphTabState })
                      _ -> t1
                  )
        NextStep -> do
          updateInfo
            $ \x ->
                flip updateGraphStep x
                  ( \s -> min ((numberOfStates x.graphTabState) - 1) (s + 1)
                  )
          handleAction UpdateGraphContent
        PrevStep -> do
          updateInfo
            $ \x ->
                flip updateGraphStep x
                  ( \s -> max (s - 1) 0
                  )
          handleAction UpdateGraphContent
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
                else do
                  H.modify_
                    $ \s -> case s.info of
                        Left _ -> s { info = Right defaultOk }
                        Right _ -> s
                  setCurrentTab
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
                                  Just cont -> Tab t { tabContent = pre_ [ text cont ] }
                                  Nothing ->
                                    (\x -> Tab t { tabContent = text x })
                                      $ case t.id of
                                          TCBNWithGraph -> "Waiting for " <> show TCBNWithGraph
                                          _ -> "No response"
                              )
                                <$> s.textTabs
                            , graphTabState = r.graphTab
                            }
                    handleAction UpdateGraphContent
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
        -- https://github.com/purescript-halogen/purescript-halogen/blob/847659d8b057ba03a992ced5b3e25e27a1e265ff/examples/keyboard-input/src/Main.purs
        -- FIXME don't catch keys when typing in an editor
        HandleKey sid ev -> do
          getActiveTabId
            >>= case _ of
                Just TCBNWithGraph -> stepStates
                _ -> pure unit
          stepTabs
          where
          stepStates
            | KE.key ev == "n" = handleAction NextStep
            | KE.key ev == "p" = handleAction PrevStep
            | otherwise = pure unit

          -- FIXME separate action
          stepTabs
            | KE.key ev == "t" = handleAction NextTab
            | otherwise = pure unit
        NextTab -> do
          st <- get >>= (\x -> pure x.currentTab)
          handleAction $ SelectTab (nextTab st)
        HandleError error -> do
          H.modify_ $ \s -> s { info = Left $ defaultError { parseError = error } }
          setStatePopovers
        NoOp -> pure unit

  forEditors :: forall output. (Editor -> Action) -> H.HalogenM State Action () output Aff Unit
  forEditors f = traverse_ handleAction (map (\e -> f e) [ EOEditor, PhiEditor ])

  setCurrentTab :: forall output. H.HalogenM State Action () output Aff Unit
  setCurrentTab = get >>= (\x -> handleAction $ SelectTab x.currentTab)

getActiveTabId ∷ ∀ (a ∷ Type). H.HalogenM State Action () a Aff (Maybe TabId)
getActiveTabId = do
  h <- get
  pure
    $ case h.info of
        Right { textTabs } -> (DA.find (\(Tab t) -> t.isActive) textTabs) <#> (\(Tab t) -> t.id)
        Left _ -> Nothing

logState ∷ ∀ (a ∷ Type). H.HalogenM State Action () a Aff Unit
logState = get >>= (\x -> log_ $ show x.info)

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

-- #Stringification
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

-- #Data
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
              , href: Just "https://github.com/objectionary/try-phi/blob/9eb9879c46decc4eb1e6f34beaa6a2736224b649/back/language-utils/phi-utils/src/Phi/Minimal/Model.hs#L156"
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
              , href: Just "https://github.com/objectionary/try-phi/blob/9eb9879c46decc4eb1e6f34beaa6a2736224b649/back/language-utils/phi-utils/src/Phi/Minimal/Model.hs#L183"
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
              , href: Just "https://github.com/objectionary/try-phi/blob/9eb9879c46decc4eb1e6f34beaa6a2736224b649/back/language-utils/phi-utils/src/Phi/Minimal/Pretty.hs#L92"
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
              , href: Just "https://github.com/objectionary/try-phi/blob/9eb9879c46decc4eb1e6f34beaa6a2736224b649/back/language-utils/phi-utils/src/Phi/Minimal/Pretty.hs#L100"
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
              , href: Just "https://github.com/objectionary/try-phi/blob/9eb9879c46decc4eb1e6f34beaa6a2736224b649/back/language-utils/phi-utils/src/Phi/Minimal/Pretty.hs#L146"
              , txt: Just "Code"
              }
            , { pref: Just "It does apply"
              , href: Just "https://drive.google.com/open?id=1ZxlI0npXn4qLQj9hzCQtH3-O5xnrAsJH&disco=AAAASJgTzOw"
              , txt: Just "Decorated Instantiation"
              }
            , { pref: Just "Press the button [Next step] or the key [n] to proceed to the next step"
              , href: Nothing
              , txt: Nothing
              }
            , { pref: Just "Press the button [Previous step] or the key [p] to return to the previous step"
              , href: Nothing
              , txt: Nothing
              }
            ]
        }
      , { x: getName TPhiLatex
        , y:
            [ { pref: Nothing
              , href: Just "https://github.com/objectionary/eo/blob/546279ffc483e1be7decb85ad7e067631fbe8d72/paper/sections/calculus.tex#L347"
              , txt: Just "Sample Latex"
              }
            , { pref: Just "Define a"
              , href: Just "https://www.overleaf.com/learn/latex/Commands#Simple-Commands"
              , txt: Just "new command"
              }
            , { pref: Just "With a new command, you may override the phi-specific commands"
              , href: Nothing
              , txt: Nothing
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

allTabs :: OkState -> Array Tab
allTabs st = st.textTabs

textTabIds ∷ Array TabId
textTabIds = [ TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP, TCBNWithGraph, TPhiLatex ]

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

-- #State
defaultOk :: OkState
defaultOk =
  { textTabs:
      ( \( Tuple
            ( Tuple a b
          )
            c
        ) ->
          Tab { id: a, buttonText: b, isActive: c, tabContent: div_ [] }
      )
        <$> DA.zip (DA.zip textTabIds btexts) isActives
  , graphTabState:
      GraphTab
        { states: []
        , graphs: []
        , step: 0
        }
  }
  where
  btexts = [ "Phi term", " WHNF", "NF", "CBN Reduction", "CBN With TAP", "CBN With Graph", "LaTeX-ed term" ]

  isActives = [ true, false, false, false, false, false, false ]

defaultError :: ErrorState
defaultError =
  { errorTab: Tab { id: errorTabId, isActive: isActive, buttonText: bText, tabContent: div_ [] }
  , parseError: NoCode
  }
  where
  bText = "Errors"

  isActive = true

md1 :: State
md1 =
  { currentEditor: EOEditor
  , currentTab: TTerm
  , info: Left defaultError
  }

-- #Logic
-- FIXME name
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
  ppParseError e = div_ [ pre [ style "white-space: pre-wrap" ] [ text $ getMessage e ] ]

  ppErrorMessage :: forall b. ParseError -> HTML b Action
  ppErrorMessage e = div [ U.classes_ [] ] [ h3_ [ span [ style "font-weight:normal" ] [ text $ getEditor e ] ] ]

  showError :: forall b. ParseError -> HTML b Action
  showError e = div [ U.classes_ [ "d-flex" ] ] [ div_ [ ppErrorMessage e, ppParseError e ] ]

  -- FIXME
  t' = Tab t { tabContent = showError pe }

nextTab :: TabId -> TabId
nextTab i = case findIndex ((==) i) textTabIds of
  Just i' -> case textTabIds !! ((i' + l + 1) `mod` l) of
    Just i'' -> i''
    _ -> i
  _ -> i
  where
  l = length textTabIds

-- use lens and take OkState
-- updateGraphStep :: (Int -> Int) -> GraphTab -> GraphTab
-- updateGraphStep f (GraphTab g) = GraphTab g { step = f g.step }
updateGraphStep :: (Int -> Int) -> OkState -> OkState
updateGraphStep f ok@{ graphTabState: (GraphTab g) } = ok { graphTabState = GraphTab g { step = f g.step } }

numberOfStates :: GraphTab -> Int
numberOfStates (GraphTab { states }) = length states

-- #Props
propDataBs ∷ ∀ a b. String → String → IProp a b
propDataBs propName value = prop ("data-bs-" <> propName) value

propAria ∷ ∀ a b. String → String → IProp a b
propAria propName value = prop ("aria-" <> propName) value

prop ∷ ∀ a b. String → String → IProp a b
prop propName value = attr (AttrName propName) value

-- #HTML
html ∷ ∀ a. State -> HTML a Action
html state =
  div
    [ U.classes_ [ dGrid, "gap-1" ] ]
    $ divRow
    <$> [ cdns
      , eoLogoSection
      , buttons
      , guide
      , editorsDiv
      , infos state
      , pageFooter
      ]
  where
  divRow x = div [ U.classes_ [ "p-1" ] ] [ x ]

permalinkButton :: forall a. HTML a Action
permalinkButton =
  button
    [ type_ ButtonButton
    , U.classes_
        [ "btn", "btn-warning"
        ]
    , id permalink_
    , onClick $ \_ -> CopyToClipboard
    ]
    [ text $ "Copy permalink" ]

buttons ∷ ∀ a. HTML a Action
buttons =
  div
    [ U.classes_ [ dFlex, justifyContentCenter ]
    ]
    [ permalinkButton, guideButton ]

guideButton :: forall a. HTML a Action
guideButton =
  button
    [ type_ ButtonButton
    , U.classes_
        [ "btn", "btn-success"
        ]
    , propDataBs "toggle" "offcanvas"
    , propDataBs "target" "#offcanvasNavbar"
    , prop "aria-controls" "offcanvasNavbar"
    ]
    [ text $ "Editor guide" ]

infos :: ∀ a. State -> HTML a Action
infos s = case s.info of
  Left e -> errorTab e
  Right r -> termTabs r

errorTab :: forall a. ErrorState -> HTML a Action
errorTab { parseError: pe, errorTab: et } =
  -- TODO where to set tab content?
  div_
    [ nav_
        [ div
            [ U.classes_ [ "nav", "nav-tabs" ], id "nav-tab", HA.role "tablist" ]
            [ tabButton et' ]
        ]
    , div
        [ U.class_ "tab-content", id "nav-tabContent" ]
        [ tabContent et' ]
    ]
  where
  et' = setTab et pe

eoLogoSection ∷ ∀ a b. HTML a b
eoLogoSection =
  section_
    [ header_
        [ div [ U.classes_ [ dFlex, justifyContentCenter ] ]
            [ a [ href "https://www.eolang.org" ]
                [ img
                    [ src "https://www.yegor256.com/images/books/elegant-objects/cactus.png"
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
  i
    [ id infoId
    , U.classes_ [ "bi", "bi-info-square", "ms-2" ]
    , propDataBs "container" "body"
    , propDataBs "toggle" "popover"
    , propDataBs "placement" "top"
    , propDataBs "content" (getInfoContent infoId)
    ]
    []

editorDiv :: forall a b. Editor -> String -> HTML a b
editorDiv ed ref =
  div [ U.classes_ [ "col-sm-6" ] ]
    [ div [ U.class_ "row" ]
        [ div [ U.classes_ [ dFlex, justifyContentCenter ] ]
            [ p_
                [ a [ href ref ] [ text $ editorNamePretty ed <> " " ]
                , text "code"
                -- FIXME edit popover 
                , infoIcon $ mkInfo (editorName ed) # _editor
                ]
            ]
        ]
    , div [ U.classes_ [ dFlex, justifyContentCenter ] ]
        [ div [ U.class_ "row" ]
            [ div [ id $ editorName ed <> "-editor" ] []
            ]
        ]
    ]

editorsDiv :: forall a b. HTML a b
editorsDiv =
  div
    [ U.class_ "container-fluid", id "cont" ]
    [ div
        [ U.class_ "row" ]
        [ editorDiv PhiEditor "https://arxiv.org/abs/2204.07454"
        , editorDiv EOEditor "https://www.eolang.org"
        ]
    ]

-- pageFooter :: View action
pageFooter ∷ ∀ a b. HTML a b
pageFooter =
  nav_
    [ ul_
        [ li_
            [ text "Join our "
            , a [ href "https://t.me/polystat_org" ] [ text "Telegram group" ]
            , text " to discuss how 𝜑-calculus works"
            ]
        ]
    , ul_
        [ li_
            [ a
                [ href "https://github.com/polystat/try-phi/stargazers" ]
                [ img
                    [ src "https://img.shields.io/github/stars/polystat/try-phi.svg?style=flat-square"
                    , alt "github stars"
                    ]
                ]
            ]
        ]
    ]

cdns ∷ ∀ a b. HTML a b
cdns =
  div_
    [ link [ href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css", rel "stylesheet", type_ textCSS ]
    , script [ src "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/js/bootstrap.bundle.min.js", type_ applicationJavascript ] []
    -- TODO insert into a separate tab 
    -- TODO add tab switching with ctrl+tab
    , script [ src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@e974857b00a510ba8f1286b7d6e15c5e3f193267/docs/eo-editor.js", U.attr_ "type" "module" ] []
    , link [ href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/eo-editor@e974857b00a510ba8f1286b7d6e15c5e3f193267/docs/eo-editor.css", type_ textCSS ]
    , script [ src "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@32829a4e1b29a203ee014f3d84a18cd88edda98e/docs/phi-editor.js", U.attr_ "type" "module" ] []
    , link [ href "https://cdn.jsdelivr.net/gh/br4ch1st0chr0n3/phi-editor@32829a4e1b29a203ee014f3d84a18cd88edda98e/docs/phi-editor.css", type_ textCSS ]
    , link [ href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.0/font/bootstrap-icons.css", rel "stylesheet", type_ textCSS ]
    , link [ href "https://www.yegor256.com/images/books/elegant-objects/cactus.png", rel "shortcut icon" ]
    , link [ href "https://cdn.jsdelivr.net/gh/yegor256/tacit@gh-pages/tacit-css.min.css", rel "stylesheet", type_ textCSS ]
    ]

tabButton :: forall a. Tab -> HTML a Action
tabButton (Tab tab) =
  button
    [ U.classes_ $ [ "nav-link" ] <> active
    , HP.id $ mkButton id
    , propDataBs "toggle" "tab"
    , propDataBs "target" ("#" <> mkContent id)
    , type_ ButtonButton
    , HA.role "tab"
    , propAria "controls" (mkContent id)
    , propAria "selected" selected
    , onClick $ \_ -> SelectTab tab.id
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
  div
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

termTabs :: forall a. OkState -> HTML a Action
termTabs st =
  div_
    [ nav_
        [ div
            [ U.classes_ [ "nav", "nav-tabs" ], id "nav-tab", HA.role "tablist" ]
            (tabButton <$> allTabs st)
        ]
    , div
        [ U.class_ "tab-content", id "nav-tabContent" ]
        (tabContent <$> st.textTabs)
    ]

stepButton :: forall b. Action -> HTML b Action
stepButton a =
  button
    [ type_ ButtonButton
    , U.classes_ [ "btn", "btn-warning" ]
    , onClick $ \_ -> a
    ]
    [ text $ textStepButton a ]

getGraphTabContent :: forall b. GraphTab -> HTML b Action
getGraphTabContent (GraphTab { states, graphs, step }) =
  div [ U.class_ "row" ]
    [ div [ U.class_ "col-sm" ]
        [ stepButton PrevStep
        , stepButton NextStep
        , pre_ [ text $ maybe ("step error: " <> show step) identity (states !! step) ]
        ]
    , div [ U.class_ "col-sm" ]
        [ img
            [ src $ maybe ("step error: " <> show step) ((<>) "https://quickchart.io/graphviz?layout=dot&format=svg&graph=") (graphs !! step)
            ]
        ]
    ]

-- FIXME need to double click outside to close 
guide :: forall b. HTML b Action
guide =
  nav [ U.classes_ [ "navbar", "navbar-light" ] ]
    [ div [ U.class_ "container-fluid" ]
        [ div
            [ U.classes_ [ "offcanvas", "offcanvas-start" ]
            , prop "tabindex" "-1"
            , id "offcanvasNavbar"
            , propAria "labelledby" "offcanvasNavbarLabel"
            , propDataBs "backdrop" "true"
            ]
            [ div [ U.class_ "offcanvas-header" ]
                [ h5 [ U.class_ "offcanvas-title", id "offcanvasNavbarLabel" ]
                    [ text "Editor guide"
                    ]
                , button
                    [ type_ ButtonButton
                    , U.classes_ [ "btn-close", "text-reset" ]
                    , propDataBs "dismiss" "offcanvas"
                    , propAria "label" "Close"
                    ]
                    []
                ]
            , div [ U.class_ "offcanvas-body" ]
                [ -- ul [ U.classes_ ["nav", "nav-tabs", "mb-3"], id "nav-tab" ] --   [ text "Editor guide" --   ]
                  text "hey"
                ]
            ]
        ]
    ]

{-

<nav class="navbar navbar-light">
    <div class="container-fluid">
      <div class="offcanvas offcanvas-end" tabindex="-1" id="offcanvasNavbar" aria-labelledby="offcanvasNavbarLabel">
        <div class="offcanvas-header">
          <h5 class="offcanvas-title" id="offcanvasNavbarLabel">Editor guide</h5>
          <button type="button" class="btn-close text-reset" data-bs-dismiss="offcanvas" aria-label="Close"></button>
        </div>
        <div class="offcanvas-body">
          <ul class="nav nav-tabs mb-3" id="nav-tab" role="tablist">
            <li class="nav-item">
              <button class="nav-link active" id="keybindings-tab" data-bs-toggle="pill" data-bs-target="#keybindings"
                type="button" role="tab" aria-controls="keybindings" aria-selected="true">Keybindings</button>
            </li>
            <li class="nav-item" 0>
              <button class="nav-link" id="diagnostics-tab" data-bs-toggle="pill" data-bs-target="#diagnostics"
                type="button" role="tab" aria-controls="diagnostics" aria-selected="false">Diagnostics</button>
            </li>
            <li class="nav-item">
              <button class="nav-link" id="links-tab" data-bs-toggle="pill" data-bs-target="#links" type="button"
                role="tab" aria-controls="links" aria-selected="false">Links</button>
            </li>
          </ul>
          <div class="tab-content" id="tabContent">
            <div class="tab-pane fade show active" id="keybindings" role="tabpanel" aria-labelledby="keybindings-tab">
              Use <span class="keys"><kbd class="kbd">⌘</kbd></span> on Mac instead of <span class="keys"><kbd
                  class="kbd">Ctrl</kbd></span>
              <table class="table table-hover wide-table">
                <thead>
                  <tr>
                    <th class="table-header" scope="col">Action</th>
                    <th class="table-header" scope="col">Keybinding</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td>Indent / Dedent (selection)</td>
                    <td><span class="keys"><kbd class="kbd">Tab</kbd></span>/<span class="keys"><kbd
                          class="kbd">Shift</kbd>+<kbd class="kbd">Tab</kbd></span></td>
                  </tr>
                  <tr>
                    <td>Toggle parse tree in browser console</td>
                    <td><span class="keys"><kbd class="kbd">Ctrl</kbd>+<kbd class="kbd">Shift</kbd>+<kbd
                          class="kbd">L</kbd></span></td>
                  </tr>
                  <tr>
                    <td>Undo the last action</td>
                    <td><span class="keys"><kbd class="kbd">Ctrl</kbd>+<kbd class="kbd">Z</kbd></span></td>
                  </tr>
                </tbody>
              </table>
            </div>
            <div class="tab-pane fade" id="diagnostics" role="tabpanel" aria-labelledby="diagnostics-tab">
              <table class="table table-hover wide-table">
                <thead>
                  <tr>
                    <th class="table-header" scope="col">Diagnostic</th>
                    <th class="table-header" scope="col">Meaning</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td>Red triangle</td>
                    <td>Position of a parsing error</td>
                  </tr>
                  <tr>
                    <td>Red wavy underlines</td>
                    <td>(Possibly) a part of an incorrect expression</td>
                  </tr>
                </tbody>
              </table>
            </div>
            <div class="tab-pane fade" id="links" role="tabpanel" aria-labelledby="links-tab">
              <table class="table table-hover wide-table">
                <tbody>
                  <tr>
                    <td><a href="https://github.com/br4ch1st0chr0n3/eo-editor">GitHub repo</a></td>
                  </tr>
                  <tr>
                    <td><a href="https://github.com/cqfn/eo">EO repo</a></td>
                  </tr>
                  <tr>
                    <td>
                      We use <a href="https://codemirror.net/6/">Codemirror 6</a>
                    </td>
                  </tr>
                  <tr>
                    <td>
                      And <a href="https://lezer.codemirror.net/docs/guide/">Lezer</a> parse system
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
    </div>
  </nav>
-}
