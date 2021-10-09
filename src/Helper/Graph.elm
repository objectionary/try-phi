module Helper.Graph exposing (..)

import Dict exposing (..)
import Html exposing (node)



-- support add edgeData, labels, convert to DOT-string


type NodeFrame
    = Circle
    | Rectangle


type NodeContent
    = Empty
    | Locator String


type alias NodeData =
    { content : NodeContent
    , frame : NodeFrame
    }


type alias EdgeLabel =
    String


type EdgeType
    = Dashed
    | Solid


type alias EdgeData =
    { label : EdgeLabel
    , edgeType : EdgeType
    }


type alias NodeId =
    Int


type alias EdgeNodes =
    ( NodeId, NodeId )


type alias Graph =
    { nodeData : Dict NodeId NodeData
    , edgeData : Dict EdgeNodes EdgeData
    , lastNode : NodeId
    }


graphSample1 : Graph
graphSample1 =
    Graph
        (Dict.fromList [ ( 1, NodeData Empty Circle ), ( 2, NodeData (Locator "^") Rectangle ), ( 3, NodeData Empty Rectangle ) ])
        (Dict.fromList [ ( ( 1, 2 ), { label = "a", edgeType = Dashed } ), ( ( 2, 3 ), { label = "b", edgeType = Solid } ) ])
        3


type alias Edge =
    { from : NodeId
    , to : NodeId
    , label : EdgeLabel
    , edgeType : EdgeType
    }


setEdge : Edge -> Graph -> Graph
setEdge edge graph =
    let
        nodes =
            ( edge.from, edge.to )

        data =
            { label = edge.label, edgeType = edge.edgeType }
    in
    { graph | edgeData = Dict.insert nodes data graph.edgeData }


type alias Node =
    { id : NodeId
    , content : NodeContent
    , frame : NodeFrame
    }


setNode : Node -> Graph -> Graph
setNode node graph =
    { graph | nodeData = Dict.insert node.id (NodeData node.content node.frame) graph.nodeData }