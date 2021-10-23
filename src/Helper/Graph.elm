module Helper.Graph exposing (..)

import Dict exposing (..)
import Html exposing (node)



-- support add edgeData, labels, convert to DOT-string


type NodeFrame
    -- ordinary node
    = Circle
    -- node with locator
    | Square
    -- node for void attributes
    | Triangle


type alias NodeLabel = String


type alias NodeData =
    { label : NodeLabel
    , frame : NodeFrame
    }


type alias EdgeLabel =
    Maybe String


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
        (Dict.fromList [ ( 1, NodeData "" Circle ), ( 2, NodeData "rho" Square ), ( 3, NodeData "" Square ) ])
        (Dict.fromList [ ( ( 1, 2 ), { label = Just "a", edgeType = Dashed } ), ( ( 2, 3 ), { label = Just "b", edgeType = Solid } ) ])
        3

emptyGraph : Graph
emptyGraph = 
    Graph Dict.empty Dict.empty 0


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
    , label : NodeLabel
    , frame : NodeFrame
    }


setNode : Node -> Graph -> Graph
setNode node graph =
    { graph | nodeData = Dict.insert node.id (NodeData node.label node.frame) graph.nodeData }

getDOT : Graph -> String
getDOT graph =
    let
        -- pattern: node[ shape = ..., label = ... ]\n
        nodeRows =
            List.foldr (++)
                ""
                (List.map
                    (\( id, node ) ->
                        List.foldr (++)
                            ""
                            [ "  "
                            , String.fromInt id
                            , "[ "
                            , "label = <" ++ node.label ++ ">"
                            , "shape = "
                            , case node.frame of
                                Triangle -> "triangle"
                                Square -> "square"
                                Circle -> "circle"
                            , " ];\n"
                            ]
                    )
                    (Dict.toList graph.nodeData)
                )

        -- pattern: node1 -> node2[ label = ..., style = ... ]\n
        edgeRows =
            List.foldr (++)
                ""
                (List.map
                    (\( ( from, to ), edge ) ->
                        List.foldr (++)
                            ""
                            [ "  "
                            , String.fromInt from
                            , " -> "
                            , String.fromInt to
                            , "[ "
                            , "label = \" "
                            , case edge.label of
                                Just label ->
                                    label

                                Nothing ->
                                    ""
                            , "\", "
                            , "style = "
                            , case edge.edgeType of
                                Dashed ->
                                    "dashed"

                                Solid ->
                                    "solid"
                            , " ];\n"
                            ]
                    )
                    (Dict.toList graph.edgeData)
                )

        -- note: directed graph
        fullGraph =
            List.foldr (++)
                ""
                [ "digraph g {\n"
                , "  node [ label = \"\", shape=circle, width=0.3];\n"
                , nodeRows
                , edgeRows
                , "}"
                ]
    in
    fullGraph