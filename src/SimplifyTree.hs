{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SimplifyTree where

import qualified ParseEO as P(pProgram, Position(..), Node(..), TokenType (..))
import Data.Text ( Text )
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Scientific (Scientific)
import GHC.Exts (IsList)
import ParseEO (Node (..))
import Control.Monad.State

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
  deriving (Eq, Ord)


data AttrValue a
  = VoidAttr
  | Attached a
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
  { getObject :: InsOrdHashMap AttrName (AttrValue a)}
  deriving (Eq, Functor, Foldable, Traversable)

data Term
  = App {a1::AttrName, a2::AttrName, t::Term, id::Id}
  | Obj {obj::Object Term, vararg::Bool, id::Id}
  | Dot {t::Term, a::AttrName, id::Id}
  | DataTerm {v::DataValue, id::Id}
  | AttrTerm {a::AttrName, isConst::Bool, imported::Maybe Text, isVarArg::Bool, id::Id}

-- for each term, we need to remember where from code it comes from
-- but can we?
-- for now, we can ignore it, I think

type MapIdTerm = InsOrdHashMap Id Term


toTerm :: Node -> State (Int, Term, MapIdTerm) ()
toTerm node = do
  (id, term, mp) <- get
  let Node token l p1 p1 = node
  let term =
        case term of
          P.pProgram ->
            case l of
              [_,_,os] ->
                
  put (id,term, mp) 