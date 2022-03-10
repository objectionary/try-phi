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
  -- s
    Name Text
  | IdName Id
  | At
  | Rho
  | Root
  | Sigma
  | Star
  | Vertex
  | Xi

  -- may be applied to @ or $

  -- ...s
  | Unpack AttrName
  -- s...
  | VarArg AttrName
  -- s'
  | Copy AttrName
  -- s.
  | InverseDot Text
  deriving (Eq, Ord, Generic)

instance Hashable AttrName

-- need for returning named applications

newtype AttrValue a = Attached a
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

-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

data Props = Props {isConst::Bool, imported::Maybe Text} deriving (Eq)

emptyProps :: Props
emptyProps = Props False Nothing

data ApplicationArgument = AppArg {optionalName::Maybe Term, value::Maybe Term} deriving (Eq)

data Term
  -- app may be to an unnamed term, so we keep a term
  -- arguments may be with an optional name, so we keep such names and terms
  = App {t1::Term, a1::[ApplicationArgument], idNum::Id}
  | Obj {freeAttrs::[Term], obj::Object Term, idNum::Id}
  | FreeAttr {a::AttrName, idNum::Id}
  -- need to keep position of attribute, so use Attr
  | Dot {t::Term, attr::Term, idNum::Id}
  | DataTerm {v::DataValue, idNum::Id}
  -- used to store attribute names and their properties
  -- for attributes of objects
  -- for arguments of vararg applications
  -- properties of attribute name should come with it
  | AttrBound {a::AttrName, isConst::Bool, imported::Maybe Text, idNum::Id}
  | Locator {n::Int, idNum::Id}
  | AttrHead {a::AttrName, idNum::Id}
  -- for a > a! /bool
  -- as an argument of application
  -- use Attr for name
  | NamedTerm {t::Term, name::Maybe Term, idNum::Id}
  deriving (Eq)

-- TODO assign a name to unnamed attributes of objects? 
-- Only to top objects, probably
-- it will hinder the analysis

emptyObject :: Term
emptyObject = Obj {obj = Object M.empty, freeAttrs = [], idNum = 0}

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
getTerm1 :: Maybe Term -> Node -> Maybe Term
getTerm1 t n = term $ evalState (toTerm t n) ()

-- | get term from no term and node
-- getTerm2 :: Node -> (Maybe AttrName, Maybe Props, Maybe Term)
getTerm2 :: Node -> Maybe Term
getTerm2 = getTerm1 Nothing

-- | evaluate conversion to term on given term and node
evalToTerm1 t n = evalState (toTerm t n) ()

-- | evaluate conversion to term of a node independent of term
evalToTerm2 = evalToTerm1 Nothing


-- | contains optional free argument name
-- and corresponding term
data Value = Value {attr::Maybe Term, term::Maybe Term}
initValue::Value
initValue = Value Nothing Nothing

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
      -- comes named term and maybe its name
      -- come props 
      let combine obj Value {term = term1, attr = a1} =
            case term1 of
              Just term2 ->
                  M.insert (
                    case a1 of
                      Just FreeAttr {a = name1} -> name1
                      Nothing -> IdName (idNum term2)
                      _ -> err l
                  ) term2 obj
              _ -> err l

      let top = Attached <$> foldl combine M.empty os
      return initValue {term = Just emptyObject {obj = Object top}}

    P.Object -> do
      -- if we enter an object, we should produce a named term possibly with props
      let objCurrent = emptyObject
      let [_, a, t, s] = l
      return initValue

    P.Abstraction -> do
      return initValue

    P.Attributes -> do
      let f Node {
              tag = P.Label,
              nodes = [Node {tag = P.NAME name}, Node {tag = dots}],
              nodeId = id'
            } = FreeAttr {a = f1 (Name name), idNum = id'}
              where
                f1
                  | dots == P.DOTS = VarArg
                  | dots == P.NothingNode = id
                  | otherwise = err l
          f _ = err l

      let attrs = fmap f l

      let obj = Obj {obj = Object M.empty, freeAttrs = attrs, idNum = i}
      return initValue {term = Just obj}

    P.Head -> do
      let [dots@Node {tag = dotsTag}, name1@Node {tag = name2, nodes = ns1}] = l
      let unpacks
            | dotsTag == P.DOTS = Unpack
            | dotsTag == P.NothingNode = id
            | otherwise = err dots

      let t' =
            case name2 of
              P.ROOT -> Root
              P.AT -> At
              P.RHO -> Rho
              P.XI -> Xi
              P.SIGMA -> Sigma
              P.STAR -> Star
              P.ListNode ->
                case ns1 of
                  [Node {tag = P.NAME name3}, n4@Node {tag = c1}] ->
                    case c1 of
                      P.COPY -> Copy (Name name3)
                      P.DOT  -> InverseDot name3
                      _ -> err n4
                  _ -> err ns1
              _ -> err name1
      return initValue {term = Just AttrHead {a = unpacks t', idNum = i}}

    P.Application -> do
      let [s, h, a1] = l
      -- in the first node
      -- there is no name for current application here
      -- whether there is Head or Application inside
      -- so we can take just term
      let s' = getTerm2 s

      -- in htail, there are just application arguments
      -- we will do application there
      -- so we can take just term
      let h' = getTerm1 s' h

      -- finally, we put current application inside app1 
      -- and pass the result up
      let Value {attr = at, term = Just obj} = evalToTerm1 h' a1
      -- if no name was provided, need to create
      -- TODO check
      return $ Value {attr = at, term = Just NamedTerm {t = obj, name = at, idNum = i}}

    P.Htail -> do
      -- here, we need to make applications with inline arguments
      -- some of them may be named, like:
      -- where 
      -- t (a:a)
      let f x@Node {tag = t1, nodes = n1} =
            case t1 of
              P.Head ->
                case n1 of
                  [h] -> evalToTerm2 h
                  _ -> err n1
              P.Application -> evalToTerm2 x
              P.Abstraction -> evalToTerm2 x
              _ -> err x
      let args = (\(Value a b) -> AppArg a b) . f <$> l
      let term1 = 
            case term of 
              Just t -> t
              _ -> err node
      return initValue {term = Just App {t1 = term1, a1 = args, idNum = i}}

    _ -> return initValue