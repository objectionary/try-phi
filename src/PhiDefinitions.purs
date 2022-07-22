module PhiDefinitions
  ( Action(..)
  , AppState(..)
  , Editor(..)
  , ErrorState
  , GraphTab(..)
  , HandleError(..)
  , NewCode
  , OkState
  , ParseError(..)
  , Request(..)
  , Response(..)
  , State
  , Tab(..)
  , TabId(..)
  , TabMode(..)
  , Tag(..)
  , Term(..)
  , TextTabs
  , class IdGettable
  , class LogError
  , err
  , getId
  , getName
  , log_
  , textTabIds
  )
  where

import Data.Generic.Rep
import Prelude

import Data.Argonaut (class EncodeJson, JsonDecodeError(..), jsonEmptyObject, (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML (HTML, div_)
import Data.Show.Generic(class GenericShow)

-- / Types /

data AppState = DevState | DeployState

data Editor = EOEditor | PhiEditor

data GraphTab = GraphTab
  { states :: Array String,
    graphs :: Array String,
    step :: Int
  }

data Request = Request { code :: String}

data Response
  = OkResponse
      { code :: String,
        textTabs :: TextTabs,
        graphTab :: GraphTab
      }
  | ErrorResponse
      { error :: String
      }



data HandleError = 
    NoSnippetOrEditor 
  | NoPermalink
  | NoResponse
  | NoHrefPermalink
  | EditorError Editor String

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
  | UpdateGraphContent


data Term = Term String


-- | TODO EO and Phi tabs are synced
-- | Phi code is translated into EO code (and vice-versa) on a server
-- | Currently, the state will store just the parse errors, including
-- TODO provide error message
data ParseError = EOParseError String | PhiParseError String | NoCode


data TabMode = Active | Disabled

data TabId = TTerm | TWHNF | TNF | TCBNReduction | TCBNWithTAP | TCBNWithGraph | TError

derive instance Generic TabId _

data Tab = Tab {
  id :: TabId,
  buttonText :: String,
  isActive :: Boolean,
  tabContent :: forall a. HTML a Action
}


data Tag = OkTag | ErrorTag
instance Show Tag where
  show OkTag = "ok"
  show ErrorTag = "error"

type NewCode = {
  newCode :: String
}
type TextTabs = Map TabId String


type OkState = {
  textTabs :: Array Tab,
  graphTabState :: GraphTab
}

type ErrorState = {
  -- FIXME? parseerror is implicitly stored in tab
  parseError :: ParseError,
  errorTab :: Tab
}

-- FIXME store the last editor even in case of parse errors
-- then no need to store a flag about non-empty code, can use just an error
type State = {
  currentEditor :: Editor,
  info :: Either ErrorState OkState
}

-- / Classes /

class LogError a where
  err :: forall output. a -> H.HalogenM State Action () output Aff Unit

class IdGettable a where
  getName :: a -> String
  getId :: String -> Maybe a

-- / Instances /

instance IdGettable TabId where
  getName TTerm = "original_term"
  getName TWHNF = "whnf"
  getName TNF = "nf"
  getName TCBNReduction = "cbn_reduction"
  getName TCBNWithTAP = "cbn_with_tap"
  getName TCBNWithGraph = "cbn_with_graph"
  getName TError = "error"
  getId x
    | x == "original_term" = Just TTerm
    | x == "whnf" = Just TWHNF
    | x == "nf" = Just TNF
    | x == "cbn_reduction" = Just TCBNReduction
    | x == "cbn_with_tap" = Just TCBNWithTAP
    | x == "cbn_with_graph" = Just TCBNWithGraph
    | x == "error" = Just TError
    | otherwise = Nothing


instance Show ParseError where
  show (EOParseError s) = s
  show (PhiParseError s) = s
  show NoCode = "no code"


derive instance Eq TabId
derive instance Ord TabId


instance Show Term where
  show (Term s) = s


instance Eq Tab where
  eq (Tab t1) (Tab t2) = t1.id == t2.id

derive instance Eq Editor


instance Show Response where
  show (OkResponse r) = "OkResponse:\n" <> show r
  show (ErrorResponse r) = "ErrorResponse:\n" <> show r

instance Show TabId where
  show = getName

instance Show GraphTab where
  show (GraphTab t) = show t

instance EncodeJson Request where
  encodeJson (Request r) = 
      "code" := r.code
      ~> jsonEmptyObject

-- Type operators for Sum and Product
infixl 6 type Sum as :+:
infixl 7 type Product as :*:

-- Operator for the Product data constructor
infixl 7 Product as :*:

type TabRep = TabId :*: String :*: Boolean

derive instance Generic GraphTab _

-- https://harry.garrood.me/blog/write-your-own-generics/#representing-data-types-as-sums-of-products
instance Generic Tab TabRep where
  from (Tab {id, buttonText, isActive}) = id :*: buttonText :*: isActive
  to (id :*: buttonText :*: isActive) = (Tab {id, buttonText, isActive, tabContent : div_ []})

instance Show Tab where
  show t = 
    let 
      id :*: buttonText :*: isActive = from t 
    in 
      show {id, buttonText, isActive}

instance DecodeJson GraphTab where
  decodeJson json = do
    x <- decodeJson json
    states <- x .: "states"
    graphs <- x .: "graphs"
    pure $ GraphTab {
      states : states,
      graphs : graphs,
      -- TODO check isn't triggered occasionally
      step : 0
    }


instance DecodeJson TabId where
  decodeJson json = do
    x <- decodeString json
    case getId x of
      Just i -> Right i
      Nothing -> Left $ TypeMismatch "not a tab id"

instance DecodeJson Response where
  decodeJson json = do
    x <- decodeJson json
    tag <- x .: "tag"
    resp <- 
      if tag == show OkTag
      then 
        do
          code <- x .: "code"
          textTabs <- x .: "textTabs"
          ps <- traverse (\y -> (\z -> Tuple y z) <$> textTabs .: (getName y)) textTabIds
          let ps' = fromFoldable ps
          graphTab <- x .: "graphTab"
          pure $ OkResponse {code: code, textTabs: ps', graphTab: graphTab}
      else
        do
          e <- x .: "error"
          pure $ ErrorResponse {error : e}
    pure resp


instance LogError HandleError where
  err NoSnippetOrEditor = log_ "check 'snippet' or 'editor'"
  err NoPermalink = log_ "no permalink"
  err NoResponse = log_ "no response received!"
  err NoHrefPermalink = log_ "no href in permalink!"
  err (EditorError _ s) = log_ s

textTabIds ∷ Array TabId
-- FIXME exclude cbnwithGraph
textTabIds = [ TTerm, TWHNF, TNF, TCBNReduction, TCBNWithTAP]

log_ :: forall output. String -> H.HalogenM State Action () output Aff Unit
log_ = H.liftEffect <<< log
