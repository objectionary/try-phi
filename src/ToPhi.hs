{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}

module ToPhi where


import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import GHC.Exts (IsList (..))

type Attr = String

newtype Object a = Object
  { getObject :: InsOrdHashMap Attr (AttrValue a)
  }
  deriving (Eq, Functor, Foldable, Traversable, IsList)

(.?) :: Object a -> Attr -> Maybe (AttrValue a)
Object o .? a = InsOrdHashMap.lookup a o

(.=) :: Object a -> (Attr, a) -> Object a
Object o .= (a, v) = Object (InsOrdHashMap.insert a (Attached v) o)

data AttrValue a
  = VoidAttr
  | Attached a
  deriving (Eq, Functor, Foldable, Traversable)

splitAttrs :: Object a -> ([Attr], [(Attr, a)])
splitAttrs = foldr add ([], []) . toList
  where
    add (a, VoidAttr) (void, attached)   = (a : void, attached)
    add (a, Attached t) (void, attached) = (void, (a, t) : attached)


data DataValue =
  DataInteger Integer
  | NoData
  deriving (Eq, Show, Ord)


data Term
  = Obj (Object Term)
  | Dot Term Attr
  | App Term (Attr, Term)
  | Loc Int
  | DataTerm DataValue
  deriving (Eq)