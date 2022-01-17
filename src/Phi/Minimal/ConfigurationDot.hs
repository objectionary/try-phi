{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Phi.Minimal.ConfigurationDot where

import qualified Data.Graph.Inductive.Graph           as Graph
import           Data.Graph.Inductive.PatriciaTree    (Gr)
import qualified Data.GraphViz                        as GraphViz
import qualified Data.GraphViz.Attributes             as Attributes
import qualified Data.GraphViz.Attributes.Colors      as Colors
import qualified Data.GraphViz.Attributes.Complete    as Complete
import qualified Data.GraphViz.Printing               as GraphViz
import qualified Data.Map                             as Map
import qualified Data.Text.Lazy                       as Text
import           Phi.Minimal.Graph                    (TermEdge (..),
                                                       TermNode (..),
                                                       prettyLocator)
import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import qualified Phi.Minimal.Model                    as Model

-- | convert configuration to GraphViz DOT graph
--
-- highlight
--
-- * current node (green)
--
-- * actions (red)
--
-- * environment (blue)
toGraphviz ::
  Graph.Graph gr => CGraph.Configuration gr -> GraphViz.DotGraph Graph.Node
toGraphviz c = GraphViz.graphToDot params g
  where
    g = CGraph.graph c
    modeMap = getClassifiedNodes c
    params =
      GraphViz.nonClusteredParams
        { GraphViz.globalAttributes =
            [ GraphViz.GraphAttrs
                [ Complete.Size (Complete.GSize 100 Nothing False),
                  Complete.Layout GraphViz.Dot
                ]
            ],
          GraphViz.fmtNode = fmtTermNode modeMap,
          GraphViz.fmtEdge = fmtTermEdge modeMap
        }

currentColor :: Colors.Color
currentColor = Colors.X11Color Attributes.Chartreuse

actionColor :: Colors.Color
actionColor = Colors.X11Color Attributes.Firebrick2

parentColor :: Colors.Color
parentColor = Colors.X11Color Attributes.Blue1

width :: Attributes.Attribute
width = Attributes.penWidth 2.6

makeHightlight :: [Colors.Color] -> [Attributes.Attribute]
makeHightlight x =
  case x of
    [] -> []
    _  -> Complete.Color (Colors.toColorList x) : [width]

fmtTermEdge ::
  ModeMap -> (Graph.Node, Graph.Node, TermEdge) -> [GraphViz.Attribute]
fmtTermEdge modeMap (from, to, edge) = edgeFormat
  where
    label =
      case edge of
        DotEdge a  -> [GraphViz.toLabel ("." <> a)]
        AttrEdge a -> [GraphViz.toLabel a]
        CopyEdge   -> [GraphViz.style GraphViz.dotted]
    highlight =
      makeHightlight $
        case (Map.findWithDefault [] from modeMap, Map.findWithDefault [] to modeMap) of
          (l1, l2)
            | elem ActionNode l1 && elem ActionNode l2 -> [actionColor]
            | elem ParentNode l1 && elem ParentNode l2 -> [parentColor]
            | True -> []
    edgeFormat = label ++ highlight

fmtTermNode :: ModeMap -> (Graph.Node, TermNode) -> [GraphViz.Attribute]
fmtTermNode modeMap (node, nodeType) = nodeFormat
  where
    label =
      case nodeType of
        VoidNode     -> [GraphViz.toLabel "VOID"]
        LocDotNode _ -> [GraphViz.shape GraphViz.BoxShape]
        ObjNode      -> []
    highlight =
      makeHightlight $
        case Map.lookup node modeMap of
          Just l ->
            map
              ( \case
                  CurrentNode -> currentColor
                  ParentNode -> parentColor
                  ActionNode -> actionColor
              )
              (filter (/= ActionNode) l)
          _ -> []
    nodeFormat = label ++ highlight

data NodeMode
  = CurrentNode
  | ParentNode
  | ActionNode
  deriving (Eq)

type ModeMap = Map.Map Graph.Node [NodeMode]

addMode :: Graph.Node -> NodeMode -> ModeMap -> ModeMap
addMode node mode m = m1
  where
    l = Map.findWithDefault [] node m
    m1 = Map.insert node (mode : l) m

type Edges = Map.Map (Graph.Node, TermEdge) Graph.Node

addModeByEdge :: Graph.Node -> CGraph.Action -> Edges -> ModeMap -> ModeMap
addModeByEdge node action edges m = m1
  where
    -- proper label to search by
    -- locator edges are a subset of attribute edges

    l =
      case action of
        CGraph.DotAction (_n, a)  -> DotEdge a
        CGraph.AppAction (_n, _e) -> CopyEdge
        CGraph.LocAction (n, _i)  -> AttrEdge (prettyLocator n)
    n1 = Map.findWithDefault (-1) (node, l) edges
    m1 = addMode n1 ActionNode m

-- | Extract nodes from configuration
getClassifiedNodes :: Graph.Graph gr => CGraph.Configuration gr -> ModeMap
getClassifiedNodes c = m3
  where
    m0 = Map.empty
    CGraph.Configuration cur actions env _ = c
    currentNode =
      case cur of
        Just node -> node
        Nothing   -> -1
    gr = CGraph.graph c
    edges =
      Map.fromList (map (\(n1, n2, lab) -> ((n1, lab), n2)) (Graph.labEdges gr))
    m1 = addMode currentNode CurrentNode m0
    m2 = getClassifiedActions actions edges m1
    m3 = getClassifiedEnvironment env m2

getClassifiedActions :: [CGraph.Action] -> Edges -> ModeMap -> ModeMap
getClassifiedActions actions edges modeMap =
  case actions of
    [] -> modeMap
    ac@(CGraph.DotAction (node, _attr)) : as ->
      addMode node ActionNode $
        addModeByEdge node ac edges $ getClassifiedActions as edges modeMap
    ac@(CGraph.AppAction (node, env)) : as ->
      addMode node ActionNode $
        addModeByEdge node ac edges $
          getClassifiedEnvironment env $ getClassifiedActions as edges modeMap
    ac@(CGraph.LocAction (node, _ord)) : as ->
      addMode node ActionNode $
        addModeByEdge node ac edges $ getClassifiedActions as edges modeMap

getClassifiedEnvironment :: CGraph.Environment -> ModeMap -> ModeMap
getClassifiedEnvironment environment modeMap =
  foldl
    ( \m par ->
        addMode
          (CGraph.original par)
          ParentNode
          (getClassifiedCopies (CGraph.copies par) m)
    )
    modeMap
    environment

getClassifiedCopies :: [(Graph.Node, CGraph.Environment)] -> ModeMap -> ModeMap
getClassifiedCopies parentCopies modeMap =
  foldl
    (\m (node, env) -> addMode node ParentNode (getClassifiedEnvironment env m))
    modeMap
    parentCopies

renderAsDot :: Graph.Graph gr => CGraph.Configuration gr -> Text.Text
renderAsDot c = GraphViz.renderDot $ GraphViz.toDot $ toGraphviz c

renderAsColorfulDot :: Model.Term -> Text.Text
renderAsColorfulDot term =
  renderAsDot @Gr $ last $ CGraph.steps (CGraph.initConfiguration term)

renderList :: Model.Term -> [Text.Text]
renderList term =
  map (renderAsDot @Gr) $ CGraph.steps $ CGraph.initConfiguration term
