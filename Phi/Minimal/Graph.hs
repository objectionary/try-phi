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
  | LocDotNode (Maybe Int)
  | VoidNode
  deriving (Show, Eq)

data TermEdge
  = DotEdge Attr
  | AttrEdge Attr
  | CopyEdge
  deriving (Show, Eq)

toGraphBuilder :: DynGraph gr => Term -> GraphBuilder gr TermNode TermEdge Graph.Node
toGraphBuilder = \case
  Obj o -> do
    node <- freshNode ObjNode
    let (void, attached) = splitAttrs o
    v <- voidNode
    forM_ void $ \attr ->
      edgeFromTo node (AttrEdge attr) v
    forM_ attached $ \(attr, subterm) -> do
      sub <- toGraphBuilder subterm
      edgeFromTo node (AttrEdge attr) sub
    return node
  Loc n -> do
    node1 <- freshNode (LocDotNode (Just n))
    node2 <- freshNode (LocDotNode Nothing)
    edgeFromTo node1 (AttrEdge (prettyLocator n)) node2
    return node1
  Dot t a -> do
    node <- freshNode (LocDotNode Nothing)
    sub <- toGraphBuilder t
    edgeFromTo node (DotEdge a) sub
    return node
  App t (a, u) -> do
    node <- freshNode ObjNode
    tnode <- toGraphBuilder t
    unode <- toGraphBuilder u
    edgeFromTo node CopyEdge tnode
    edgeFromTo node (AttrEdge a) unode
    return node

prettyLocator :: Int -> String
prettyLocator n = "ρ" <> n'
  where
    n' = map toSuperscript (show n)
    toSuperscript c =
      case lookup c (zip "1234567890" "¹²³⁴⁵⁶⁷⁸⁹⁰") of
        Just c' -> c'
        _       -> c

toGraph :: DynGraph gr => Term -> (Graph.Node, gr TermNode TermEdge)
toGraph = buildGraph VoidNode . toGraphBuilder

toGraph_ :: DynGraph gr => Term -> gr TermNode TermEdge
toGraph_ = snd . toGraph

