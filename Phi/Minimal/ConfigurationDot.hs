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
import qualified Data.GraphViz.Attributes as Attributes
import qualified Data.GraphViz.Printing            as GraphViz
import qualified          Data.Text.Lazy as Text
import qualified Phi.Minimal.Model as Model
import Phi.Minimal.Graph ( TermEdge(..), TermNode(..) )
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

-- currentColor :: [Colors.Color]
currentColor :: [Colors.Color]
currentColor = [Colors.X11Color Attributes.Chartreuse]
-- actionColor :: [Colors.Color]
actionColor :: [Colors.Color]
actionColor = [Colors.X11Color Attributes.Firebrick2]
-- parentColor :: [Colors.Color]
parentColor :: [Colors.Color]
parentColor = [Colors.X11Color Attributes.Blue1]

width :: [Attributes.Attribute]
width = [Attributes.penWidth 2.6]

makeHightlight :: [Colors.Color] -> [Attributes.Attribute]
makeHightlight x =
  case x of
    [] -> []
    _ -> Complete.Color (Colors.toColorList x) : width

fmtTermEdge :: ModeMap -> (Graph.Node, Graph.Node, TermEdge) -> [GraphViz.Attribute]
fmtTermEdge modeMap (from, to, edge) = edgeFormat
  where
    label =
      case edge of
        DotEdge a  -> [GraphViz.toLabel ("." <> a)]
        AttrEdge a -> [GraphViz.toLabel a]
        CopyEdge   -> [GraphViz.style GraphViz.dotted]

    highlight = makeHightlight $
      case (Map.lookup from modeMap, Map.lookup to modeMap) of
        (Just CurrentNode, _) -> []
        (Just ActionNode, _) -> actionColor
        (Just ParentNode, Just ParentNode) -> parentColor
        _ -> []

    edgeFormat = label ++ highlight


fmtTermNode :: ModeMap -> (Graph.Node, TermNode) -> [GraphViz.Attribute]
fmtTermNode modeMap (node, nodeType) = nodeFormat
  where
    label =
      case nodeType of
        VoidNode           -> [GraphViz.toLabel "VOID"]
        LocDotNode _ -> [GraphViz.shape GraphViz.BoxShape]
        ObjNode            -> []

    highlight = makeHightlight $
        case Map.lookup node modeMap of
          Just CurrentNode -> currentColor
          Just ActionNode  -> actionColor
          Just ParentNode  -> parentColor
          _ -> []

    nodeFormat = label ++ highlight

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

renderAsDot :: Graph.Graph gr => CGraph.Configuration gr -> Text.Text
renderAsDot c = GraphViz.renderDot $ GraphViz.toDot $ toGraphviz c

renderAsColorfulDot :: Model.Term -> Text.Text
renderAsColorfulDot term = renderAsDot @Gr $ last $ CGraph.steps (CGraph.initConfiguration term)

renderList :: Model.Term -> [Text.Text]
renderList term = map (renderAsDot @Gr) $ CGraph.steps $ CGraph.initConfiguration term


-- Example
-- [x -> ^0.y, y -> ^0.z, z -> [b -> ^0.c, c -> ?](c -> ^0)].x.b