{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Phi.Minimal.ConfigurationDot where
import qualified Data.Graph.Inductive.Graph        as Graph
import qualified Data.GraphViz                     as GraphViz
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.GraphViz.Attributes.Complete as Complete
import qualified Data.GraphViz.Attributes.Colors as Colors
import qualified Data.GraphViz.Printing            as GraphViz
import           Data.Text.Lazy                    (Text)
import qualified Phi.Minimal.Model as Model
import           Phi.Minimal.Graph
import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import qualified Data.Map as Map
-- import Phi.Minimal.Graphviz (toGraphviz)

-- convert term to configuration
-- build DOT from graph
-- add highlights via current node, actions and environment
-- is it possible to find them in config? yes
-- current - green`
-- actions - red
-- environment - blue

toGraphviz :: Graph.Graph gr => CGraph.Configuration gr -> GraphViz.DotGraph Graph.Node
toGraphviz c = GraphViz.graphToDot params g
  where
    g = CGraph.graph c
    modeMap = getClassifiedNodes c
    params = GraphViz.nonClusteredParams
      { GraphViz.globalAttributes = [ GraphViz.GraphAttrs
          [ Complete.Size (Complete.GSize 100 Nothing False)
          , Complete.Layout GraphViz.Dot
          ] ]
      , GraphViz.fmtNode = fmtTermNode modeMap
      , GraphViz.fmtEdge = fmtTermEdge modeMap
      }

currentColor :: [Colors.Color]
currentColor = [Colors.RGBA 135 211 124 255]
actionColor :: [Colors.Color]
actionColor = [Colors.RGBA 246 71 71 255]
parentColor :: [Colors.Color]
parentColor = [Colors.RGBA 72 113 247 255]

fmtTermEdge :: ModeMap -> (Graph.Node, Graph.Node, TermEdge) -> [GraphViz.Attribute]
fmtTermEdge modeMap (from, to, edge) = edgeFormat
  where
    label =
      case edge of
        DotEdge a  -> [GraphViz.toLabel ("." <> a)]
        AttrEdge a -> [GraphViz.toLabel a]
        CopyEdge   -> [GraphViz.style GraphViz.dotted]

    color = flip (:) [] $ Complete.Color $ Colors.toColorList $
      case (Map.lookup from modeMap, Map.lookup from modeMap) of
        (Just CurrentNode, _) -> []
        (Just ActionNode, _) -> actionColor
        (Just ParentNode, Just ParentNode) -> parentColor
        _ -> []

    edgeFormat = label ++ []


fmtTermNode :: ModeMap -> (Graph.Node, TermNode) -> [GraphViz.Attribute]
fmtTermNode modeMap (node, nodeType) = nodeFormat
  where
    label =
      case nodeType of
        VoidNode           -> [GraphViz.toLabel "VOID"]
        LocDotNode Nothing -> [GraphViz.shape GraphViz.BoxShape]
        LocDotNode (Just n)
          -> [ GraphViz.shape GraphViz.BoxShape
            , GraphViz.toLabel ("parent " <> show n)]
        ObjNode            -> []

    color = flip (:) [] $ Complete.Color $ Colors.toColorList $
        case Map.lookup node modeMap of
          Just CurrentNode -> currentColor
          Just ActionNode  -> actionColor
          Just ParentNode  -> parentColor
          _ -> []

    nodeFormat = label ++ color

data NodeMode
  = CurrentNode
  | ParentNode
  | ActionNode

type ModeMap = Map.Map Graph.Node NodeMode

-- | Extract nodes from configuration
getClassifiedNodes :: Graph.Graph gr => CGraph.Configuration gr -> ModeMap
getClassifiedNodes c = m3
  where
    m0 = Map.empty
    CGraph.Configuration cur actions env _ = c
    currentNode =
      case cur of
        Just node -> node
        Nothing -> -1
    m1 = Map.insert currentNode CurrentNode m0
    m2 = getClassifiedActions actions m1
    m3 = getClassifiedEnvironment env m2

getClassifiedActions :: [CGraph.Action] -> ModeMap -> ModeMap
getClassifiedActions actions modeMap =
  case actions of
    [] -> modeMap
    (CGraph.DotAction (node, _attr)) : as ->
      Map.insert node ActionNode $ getClassifiedActions as modeMap
    (CGraph.AppAction (node, env)) : as ->
      Map.insert node ActionNode $ getClassifiedEnvironment env $ getClassifiedActions as modeMap
    (CGraph.LocAction (node, _ord)) : as ->
      Map.insert node ActionNode $ getClassifiedActions as modeMap

getClassifiedEnvironment :: CGraph.Environment -> ModeMap -> ModeMap
getClassifiedEnvironment environment modeMap =
  case environment of
    [] -> modeMap
    parents ->
      foldl (\m par -> Map.insert (CGraph.original par) ParentNode (getClassifiedCopies (CGraph.copies par) m)) modeMap parents

getClassifiedCopies :: [(Graph.Node, CGraph.Environment)] -> ModeMap -> ModeMap
getClassifiedCopies parentCopies modeMap =
  case parentCopies of
    [] -> modeMap
    pcs -> foldl (\m (node, env) -> Map.insert node ParentNode (getClassifiedEnvironment env m)) modeMap pcs

toDot :: Graph.Graph gr => CGraph.Configuration gr -> GraphViz.DotCode
toDot c = GraphViz.toDot (toGraphviz c)

renderConfigurationAsDot :: Graph.Graph gr => CGraph.Configuration gr -> Text
renderConfigurationAsDot c = GraphViz.renderDot (toDot c)

renderAsColorfulDot :: Model.Term -> Text
renderAsColorfulDot term = renderConfigurationAsDot @Gr $ last $ CGraph.steps (CGraph.initConfiguration term)

exMultiple :: Model.Term -> [Text]
exMultiple term = map (renderConfigurationAsDot @Gr) $ CGraph.steps $ CGraph.initConfiguration term