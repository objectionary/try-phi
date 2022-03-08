{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module SimplifyTree where

import           Control.Monad.State.Strict (State (..), evalState, execState,
                                             get, put, runState)
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as M (InsOrdHashMap, empty, insert)
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import           GHC.Base                   (undefined)
import           GHC.Exts                   (IsList)
import           GHC.Generics               (Generic)
import qualified ParseEO                    as P (Node (..), Position (..),
                                                  TokenType (..), pProgram)
import qualified Data.List                  as DL
import Text.Printf (printf)

type Id = Int

data AttrName =
    Name Text
  | At
  | Rho
  | Root
  | Sigma
  | Star
  | Vertex
  | Xi
  -- ...s
  | Dots AttrName
  -- s'
  | Copy Text
  | InverseDot Text
  deriving (Eq, Ord, Generic)

instance Hashable AttrName

-- need for returning named applications
data Props = Props {isConst::Bool, imported::Maybe Text} deriving (Eq)

data AttrValue a
  = VoidAttr
  | Attached a Props
  deriving (Eq, Functor, Foldable, Traversable)

data DataValue
  = Bool Bool
  | Byte Integer
  | Bytes [Integer]
  | Char Char
  | Float Scientific
  | Hex Integer
  | Int Integer
  | Regex Text Text
  | String Text
  | Text Text
  deriving (Eq, Ord)

-- meta, comments, license?

newtype Object a = Object
  { getObject :: M.InsOrdHashMap AttrName (AttrValue a)}
  deriving (Eq, Functor, Foldable, Traversable)


data Term
  = App {a1::AttrName, a2::Maybe AttrName, t::Term, idNum::Id}
  | Obj {obj::Object Term, hasVarArg::Bool, idNum::Id}
  | Dot {t::Term, a::AttrName, idNum::Id}
  | DataTerm {v::DataValue, idNum::Id}
  | Attr {a::AttrName, idNum::Id}
  | Locator {n::Int, idNum::Id}
  | NamedTerm {a::AttrName, t::Term}



emptyObject :: Term
emptyObject = Obj {obj = Object M.empty, hasVarArg = False, idNum = 0}
-- emptyAttrTerm = AttrTerm {a = "", isConst = False, imported = Nothing, isVarArg = False, id = 0}
-- for each term, we need to remember where from code it comes
-- but can we?
-- for now, we can ignore it, I think

-- type MapIdTerm = M.InsOrdHashMap Id Term

-- unnamed objects will be called after their indices
-- On objects with the same name will arise an error

-- toTermObject ::

{-
Object includes both Obj and Dot (via Method)
  And also App

Object
  create current empty object
  foldl with it while passing nothing as current term to
  insert new folded objects into it on each step

  next fold list in the end of an object

  is this only dot accesses?

  this object will be put into


What do I need on each stage

we will pass terms inside the state
if we need to pass the state intact, we'll see

-}

data Node = Node
  { nodeId    :: Id,
    tag :: P.TokenType,
    nodes     :: [Node],
    start     :: P.Position,
    end       :: P.Position
  }

err :: Show a1 => a1 -> a2
err i = error $ printf "problem at node\n%s" (show i)

tab :: String
tab = "|  "

type TabNumber = Int

printTree :: TabNumber -> Node -> String
printTree n Node {..} =
  DL.intercalate "" (replicate n tab)
  <>
  printf "%d " nodeId
  <>
  case tag of
    P.JustNode -> printf "%s\n" (show tag)
    P.ListNode -> printf "%s\n" (show tag)
    P.NothingNode -> printf "%s\n" (show tag)
    _ -> printf "%s [%s..%s]\n" (show tag) (show start) (show end)
    <> foldl (\s a -> s <> printTree (n + 1) a) "" nodes

instance Show Node where
  show n = printTree 0 n

enumerateNodes :: P.Node -> Node
enumerateNodes tr = evalState (enumerateNodesStep tr) 0

enumerateNodesStep :: P.Node -> State Int Node
enumerateNodesStep n@P.Node {..} = do
  c <- get
  put (c+1)
  ns <- mapM enumerateNodesStep nodes
  return $ Node {nodeId = c, tag = nodeToken, nodes = ns, start = start, end = end}

type IdNodeMap = InsOrdHashMap Int Node

getIdNodes :: Node -> IdNodeMap
getIdNodes n = execState (getIdNodesStep n) M.empty

getIdNodesStep :: Node -> State IdNodeMap ()
getIdNodesStep n@Node {..} = do
  mp <- get
  put (M.insert nodeId n mp)
  mapM_ getIdNodesStep nodes

idNodes :: P.Node -> IdNodeMap
idNodes = getIdNodes . enumerateNodes

-- | get term from term and node
-- getTerm1 :: Maybe Term -> Node -> (Maybe AttrName, Maybe Props, Maybe Term)
getTerm1 :: Maybe Term -> Node -> Value
getTerm1 t n = evalState (toTerm t n) ()

-- | get term from no term and node
-- getTerm2 :: Node -> (Maybe AttrName, Maybe Props, Maybe Term)
getTerm2 :: Node -> Value
getTerm2 = getTerm1 Nothing

data Value = Value {attr::Maybe AttrName, props::Maybe Props, term::Maybe Term}
initValue = Value Nothing Nothing Nothing

-- if it's an optional node
-- it's either NothingNode or actual node

toTerm :: Maybe Term -> Node -> State () Value
toTerm term node = do
  let Node {nodeId = i, tag = tok, nodes = l} = node

  case tok of
    P.Program -> do
      case l of
        [_, _, os@Node {tag = P.Objects}] -> toTerm Nothing os
        _ -> err l

    P.Objects -> do
      os <- mapM (toTerm Nothing) l
      let combine obj Value {attr = Just name, props = Just props, term = Just o} =
            M.insert name (Attached o props) obj
          combine _ _ =
            err node
      let top = foldl combine M.empty os
      return initValue {term = Just emptyObject {obj = Object top}}

    P.Object -> do
      -- if we enter an object, we should produce a named term possibly with props
      let objCurrent = emptyObject
      let [_, a, t, s] = l
      return initValue

    P.Abstraction -> do
      return initValue

    P.Attributes -> do
      let [as] = l

      let (attrs, hasVarArg') =
            case as of
              Node {tag = P.NothingNode} -> (M.empty, False)
              Node {tag = P.Label, nodes = ns} ->
                foldl f (M.empty, False) ns
                    where
                      f (mp, hv) Node {nodes = Node {tag = P.NAME name}:ns1} =
                        case ns1 of
                          [] -> (mp', hv)
                          -- if a name is followed by dots
                          [Node {tag = P.DOTS}] ->
                            (mp', True)
                          _ -> err as
                        where
                          mp' = M.insert (Name name) VoidAttr mp
                      f _ _ = err as
              _ -> err as

      let obj = Obj {obj = Object attrs, hasVarArg = hasVarArg', idNum = i}
      return initValue {term = Just obj}

    P.Head -> do
      let [dots@Node {tag = dotsTag}, name1@Node {nodes = Node {tag = name'}:xs}] = l
      let dots' =
            case dotsTag of 
              P.NothingNode -> id
              P.DOTS -> Dots
              _ -> err dots
      let t' = 
            case name' of
              P.ROOT -> Root
              P.AT -> At
              P.RHO -> Rho
              P.XI -> Xi
              P.SIGMA -> Sigma
              P.STAR -> Star
              P.NAME name ->
                let f =
                      case xs of
                        [] -> Name
                        [Node {tag = P.COPY}] -> Copy
                        [Node {tag = P.DOT }] -> InverseDot
                        _ -> err xs
                in f name
              _ -> err name1
      return initValue {term = Just Attr {a = dots' t', idNum = i}}

    P.Application -> do
      let [s, h, a1] = l
      -- if state contains a term, we need to combine it with current application
      -- if we meet a dot inside head, we need to return a term
      -- we can only return a term if we make Dot with folded rest of application
      -- let n1 = nodeId node
      -- let cont = toTerm rest of application
      -- or save as inverse dot and then transform terms

      -- deal with head or application
      -- there is no name for current application here
      -- whether there is Head or Application inside
      -- so we can take just term
      let Value {term = s'} = getTerm2 s
      -- deal with htail
      -- in htail, there are just application arguments
      -- we will do application there
      -- so we don't care about name and props coming from there 
      let Value {term = h'} = getTerm1 s' h
      -- finally, we put current application inside app1 
      -- and pass the result up
      return $ getTerm1 h' a1 {nodeId = i}

    P.Htail -> do
      -- here, we need to make applications with arguments
      let f t x =
            case x of
              Node {tag = P.ListNode, nodes = x:xs} ->
                case x of
                  Node {tag = P.Head, nodes = [h]} ->
                    getTerm2 h
              _ -> err x

      -- we can fold term with getTerm
      -- let args = mapM ( -> f) (t a)
      return initValue

    _ -> return initValue




      -- return (Nothing, Nothing, Nothing)
      -- abstraction returns
      -- object name or name generated from id

      -- as a term an object with free attributes added
      -- create an object, put free attributes as void into it, save object name if present
      -- Application is inline except for bytes?
      -- return Nothing

-- f = P.Node P.AT [] (P.Position 3 4) (P.Position 3 4)
-- g :: IO ()
-- g = do
--   let k = execState (toTerm f) (MyState 0 M.empty Nothing)
--   print ""
{-
Object [10:1..10:15]
|  ListNode
|  Application [10:1..10:15]
|  |  Head [10:1..10:7]
|  |  |  NothingNode
|  |  |  ListNode
|  |  |  |  Data [10:1..10:7]
|  |  |  |  |  FLOAT 500.43 [10:1..10:7]
|  |  NothingNode
|  |  Application1 [10:7..10:15]
|  |  |  ListNode
|  |  |  |  Method [10:7..10:9]
|  |  |  |  |  AT [10:8..10:9]
|  |  |  |  NothingNode
|  |  |  |  Application1 [10:9..10:15]
|  |  |  |  |  ListNode
|  |  |  |  |  |  Suffix [10:9..10:15]
|  |  |  |  |  |  |  Label [10:12..10:15]
|  |  |  |  |  |  |  |  NAME "one" [10:12..10:15]
|  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  NothingNode
|  |  |  |  |  |  Application1 [10:15..10:15]
|  |  |  |  |  |  |  ListNode
|  NothingNode
|  ListNode

500.43.@ > one
Application
unpack until suffix



Object [12:1..13:24]
|  ListNode
|  |  ListNode
|  |  |  COMMENT " This is just a simple string" [12:1..12:31]
|  |  |  INDENT 0 [12:31..13:1]
|  Application [13:1..13:24]
|  |  Head [13:1..13:15]
|  |  |  NothingNode
|  |  |  ListNode
|  |  |  |  Data [13:1..13:15]
|  |  |  |  |  STRING "Hello, \1076\1088\1091\1075!" [13:1..13:15]
|  |  NothingNode
|  |  Application1 [13:15..13:24]
|  |  |  ListNode
|  |  |  |  Suffix [13:15..13:24]
|  |  |  |  |  Label [13:18..13:23]
|  |  |  |  |  |  NAME "hello" [13:18..13:23]
|  |  |  |  |  |  NothingNode
|  |  |  |  |  JustNode
|  |  |  |  |  |  CONST [13:23..13:24]
|  |  |  |  NothingNode
|  |  |  |  Application1 [13:24..13:24]
|  |  |  |  |  ListNode
|  NothingNode
|  ListNode

# This is just a simple string
"Hello, \1076\1088\1091\1075!" > hello!

Object [15:1..15:22]
|  ListNode
|  Abstraction [15:1..15:22]
|  |  Attributes [15:1..15:10]
|  |  |  JustNode
|  |  |  |  ListNode
|  |  |  |  |  Label [15:2..15:4]
|  |  |  |  |  |  NAME "tt" [15:2..15:4]
|  |  |  |  |  |  NothingNode
|  |  |  |  |  Label [15:5..15:9]
|  |  |  |  |  |  NAME "a" [15:5..15:6]
|  |  |  |  |  |  JustNode
|  |  |  |  |  |  |  DOTS [15:6..15:9]
|  |  JustNode
|  |  |  ListNode
|  |  |  |  Suffix [15:10..15:17]
|  |  |  |  |  Label [15:13..15:17]
|  |  |  |  |  |  NAME "atom" [15:13..15:17]
|  |  |  |  |  |  NothingNode
|  |  |  |  |  NothingNode
|  |  |  |  JustNode
|  |  |  |  |  NAME "int" [15:19..15:22]
|  NothingNode
|  ListNode

[tt a...] > atom /int

Object [34:7..36:15]
|  ListNode
|  Application [34:7..34:9]
|  |  Head [34:7..34:9]
|  |  |  NothingNode
|  |  |  ListNode
|  |  |  |  NAME "z" [34:7..34:8]
|  |  |  |  JustNode
|  |  |  |  |  COPY [34:8..34:9]
|  |  NothingNode
|  |  Application1 [34:9..34:9]
|  |  |  ListNode
|  JustNode
|  |  Tail [34:9..36:15]
|  |  |  Object [35:9..36:15]
|  |  |  |  ListNode
|  |  |  |  Application [35:9..35:12]
|  |  |  |  |  Head [35:9..35:10]
|  |  |  |  |  |  NothingNode
|  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  NAME "z" [35:9..35:10]
|  |  |  |  |  |  |  NothingNode
|  |  |  |  |  JustNode
|  |  |  |  |  |  Htail [35:10..35:12]
|  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  Head [35:11..35:12]
|  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  Data [35:11..35:12]
|  |  |  |  |  |  |  |  |  |  |  FLOAT 5.0 [35:11..35:12]
|  |  |  |  |  Application1 [35:12..35:12]
|  |  |  |  |  |  ListNode
|  |  |  |  JustNode
|  |  |  |  |  Tail [35:12..36:15]
|  |  |  |  |  |  Object [36:11..36:15]
|  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  Application [36:11..36:15]
|  |  |  |  |  |  |  |  Head [36:11..36:15]
|  |  |  |  |  |  |  |  |  JustNode
|  |  |  |  |  |  |  |  |  |  DOTS [36:11..36:14]
|  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  NAME "z" [36:14..36:15]
|  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  Application1 [36:15..36:15]
|  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  ListNode
|  |  |  |  ListNode
|  ListNode

z'
  z 5
    ...z

Object [66:1..66:38]
|  ListNode
|  Abstraction [66:1..66:38]
|  |  Attributes [66:1..66:4]
|  |  |  JustNode
|  |  |  |  ListNode
|  |  |  |  |  Label [66:2..66:3]
|  |  |  |  |  |  NAME "x" [66:2..66:3]
|  |  |  |  |  |  NothingNode
|  |  JustNode
|  |  |  ListNode
|  |  |  |  Htail [66:4..66:38]
|  |  |  |  |  ListNode
|  |  |  |  |  |  Application [66:6..66:20]
|  |  |  |  |  |  |  Head [66:6..66:7]
|  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  NAME "x" [66:6..66:7]
|  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  Application1 [66:7..66:20]
|  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  Method [66:7..66:11]
|  |  |  |  |  |  |  |  |  |  NAME "add" [66:8..66:11]
|  |  |  |  |  |  |  |  |  JustNode
|  |  |  |  |  |  |  |  |  |  Htail [66:11..66:13]
|  |  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  |  Head [66:12..66:13]
|  |  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  |  |  |  Data [66:12..66:13]
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  FLOAT 1.0 [66:12..66:13]
|  |  |  |  |  |  |  |  |  Application1 [66:13..66:20]
|  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  Suffix [66:13..66:20]
|  |  |  |  |  |  |  |  |  |  |  |  Label [66:16..66:20]
|  |  |  |  |  |  |  |  |  |  |  |  |  NAME "succ" [66:16..66:20]
|  |  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  Application1 [66:20..66:20]
|  |  |  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  ListNode
|  |  |  |  |  |  Application [66:23..66:37]
|  |  |  |  |  |  |  Head [66:23..66:24]
|  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  NAME "x" [66:23..66:24]
|  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  Application1 [66:24..66:37]
|  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  Method [66:24..66:28]
|  |  |  |  |  |  |  |  |  |  NAME "sub" [66:25..66:28]
|  |  |  |  |  |  |  |  |  JustNode
|  |  |  |  |  |  |  |  |  |  Htail [66:28..66:30]
|  |  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  |  Head [66:29..66:30]
|  |  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  |  |  |  Data [66:29..66:30]
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  FLOAT 1.0 [66:29..66:30]
|  |  |  |  |  |  |  |  |  Application1 [66:30..66:37]
|  |  |  |  |  |  |  |  |  |  ListNode
|  |  |  |  |  |  |  |  |  |  |  Suffix [66:30..66:37]
|  |  |  |  |  |  |  |  |  |  |  |  Label [66:33..66:37]
|  |  |  |  |  |  |  |  |  |  |  |  |  NAME "prev" [66:33..66:37]
|  |  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  NothingNode
|  |  |  |  |  |  |  |  |  |  |  Application1 [66:37..66:37]
|  |  |  |  |  |  |  |  |  |  |  |  ListNode

[x] (x.add 1 > succ) (x.sub 1 > prev)

-}
