{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Phi.Minimal.Graphviz where

import qualified Data.Graph.Inductive.Graph        as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.GraphViz                     as GraphViz 
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import qualified Data.GraphViz.Printing            as GraphViz
import           Data.Text.Lazy                    (Text)
import           Phi.Minimal.Graph
import           Phi.Minimal.Model                 (Term)

toGraphviz :: Term -> GraphViz.DotGraph Graph.Node
toGraphviz = GraphViz.graphToDot params . (toGraph_ @Gr)
  where
    params = GraphViz.nonClusteredParams
      { GraphViz.globalAttributes = [ GraphViz.GraphAttrs
          [ GraphViz.Size (GraphViz.GSize 100 Nothing False)
          , GraphViz.Layout GraphViz.Dot
          ] ]
      , GraphViz.fmtNode = fmtTermNode
      , GraphViz.fmtEdge = fmtTermEdge
      }

fmtTermEdge :: (Graph.Node, Graph.Node, TermEdge) -> [GraphViz.Attribute]
fmtTermEdge (_from, _to, edge) =
  case edge of
    DotEdge a  -> [GraphViz.toLabel ("." <> a)]
    AttrEdge a -> [GraphViz.toLabel a]
    CopyEdge   -> [GraphViz.style GraphViz.dotted]

fmtTermNode :: (Graph.Node, TermNode) -> [GraphViz.Attribute]
fmtTermNode (_n, node) =
  case node of
    VoidNode           -> [GraphViz.toLabel "VOID"]
    LocDotNode Nothing -> [GraphViz.shape GraphViz.BoxShape]
    LocDotNode (Just n)
      -> [ GraphViz.shape GraphViz.BoxShape
         , GraphViz.toLabel ("parent " <> show n)]
    ObjNode            -> []

toDot :: Term -> GraphViz.DotCode
toDot = GraphViz.toDot . toGraphviz

renderAsDot :: Term -> Text
renderAsDot = GraphViz.renderDot . toDot