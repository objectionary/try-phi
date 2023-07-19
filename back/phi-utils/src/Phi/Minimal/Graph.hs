{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Phi.Minimal.Graph where

import           Phi.Minimal.Model
import           Phi.Utils.GraphBuilder

import           Control.Monad              (forM_)
import           Data.Graph.Inductive.Graph (DynGraph)
import qualified Data.Graph.Inductive.Graph as Graph


data TermNode
  = ObjNode
  | VoidNode
  | SomeNode
  deriving (Eq)

data TermEdge
  = DotEdge Attr
  | AttrEdge Attr
  | CopyEdge
  | LocEdge Int
  | DataEdge DataValue
  deriving (Show, Eq, Ord)

toGraphBuilder ::
     DynGraph gr => Term -> GraphBuilder gr TermNode TermEdge Graph.Node
toGraphBuilder =
  \case
    Obj o -> do
      node <- freshNode ObjNode
      let (void, attached) = splitAttrs o
      v <- voidNode
      forM_ void $ \attr -> edgeFromTo node (AttrEdge attr) v
      forM_ attached $ \(attr, subterm) -> do
        sub <- toGraphBuilder subterm
        edgeFromTo node (AttrEdge attr) sub
      return node
    -- TO-DO put locators, data, not strings on edges
    Loc n -> do
      node1 <- freshNode SomeNode
      node2 <- freshNode SomeNode
      edgeFromTo node1 (LocEdge n) node2
      return node1
    Dot t a -> do
      node <- freshNode SomeNode
      tnode <- toGraphBuilder t
      edgeFromTo node (DotEdge a) tnode
      return node
    App t (a, u) -> do
      node <- freshNode ObjNode
      tnode <- toGraphBuilder t
      unode <- toGraphBuilder u
      edgeFromTo node CopyEdge tnode
      edgeFromTo node (AttrEdge a) unode
      return node
    DataTerm t -> do
      node1 <- freshNode SomeNode
      node2 <- freshNode SomeNode
      edgeFromTo node1 (DataEdge t) node2
      return node1

