{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SimplifyTree where

import ParseEO (pProgram, Position(..))
import Data.Text ( Text )
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Scientific (Scientific)
import GHC.Exts (IsList)


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