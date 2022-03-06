{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module SimplifyTree where

import qualified ParseEO as P(pProgram, Position(..), Node(..), TokenType (..))
import Data.Text ( Text )
import qualified          Data.HashMap.Strict.InsOrd as M(InsOrdHashMap, empty, insert)
import Data.Scientific (Scientific)
import GHC.Exts (IsList)
import GHC.Base (undefined)
import Control.Monad.State.Strict (get, State(..), runState, evalState, execState, put)
import GHC.Generics (Generic)
import Data.Hashable ( Hashable )

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
  deriving (Eq, Ord, Generic)

instance Hashable AttrName

data Props = AttrProps {isConst::Bool, imported::Maybe Text, isVarArg::Bool, id::Id} deriving (Eq)

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
  = App {a1::AttrName, a2::AttrName, t::Term, idNum::Id}
  | Obj {obj::Object Term, vararg::Bool, idNum::Id}
  | Dot {t::Term, a::AttrName, idNum::Id}
  | DataTerm {v::DataValue, idNum::Id}
  -- helper term
  | NamedTerm {key::AttrName, value::AttrValue Term}

emptyObj = Obj (Object M.empty) False 0
-- emptyAttrTerm = AttrTerm {a = "", isConst = False, imported = Nothing, isVarArg = False, id = 0}
-- for each term, we need to remember where from code it comes
-- but can we?
-- for now, we can ignore it, I think

type MapIdTerm = M.InsOrdHashMap Id Term

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

data MyState = MyState {idNum :: Id, idTerm :: MapIdTerm, term :: Maybe Term}

toTerm :: P.Node -> State MyState ()
toTerm node = do
  MyState i mp t <- get
  -- reserve id
  let st1 = MyState (i+1) mp t
  put st1
  let P.Node tok l p1 p2 = node
  case tok of
    P.Program -> do
      case l of
        [_, _, os@P.Node {nodeToken = P.Objects}] -> do
          toTerm os
    P.Objects -> do
      let insertInto obj o = obj3
            where
              -- after insertion, State should contain Object term
              -- get name and converted object as an attached attribute
              MyState{term = Just (NamedTerm k at@(Attached v props))} = execState (toTerm o) obj
              -- take map from current state
              MyState{term = Just obj2@Obj {obj = Object obj1}} = obj
              -- update current state by inserting a new object as an attribute
              obj3 = obj {term = Just obj2 {obj = Object $ M.insert k at obj1}}
      st <- get
      let i = (idNum::MyState -> Id) st
      let st1 = st {term = Just Obj {obj = Object M.empty, vararg = False, idNum=i}}
      let obs = foldl insertInto st1 l
      put obs
      return ()
    -- P.Object -> 
        
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
    -- P.Program -> do
    --   return ()
    -- P.Object -> do 
    --   return ()
  return ()

f = P.Node P.AT [] (P.Position 3 4) (P.Position 3 4)
g :: IO ()
g = do 
  let k = execState (toTerm f) (MyState 0 M.empty Nothing)
  print ""
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

-}