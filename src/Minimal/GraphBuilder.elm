module Minimal.GraphBuilder exposing (..)

import Dict exposing (Dict)
import Helper.Graph as G exposing (EdgeType(..))
import Html.Attributes exposing (name)
import Minimal.Parser exposing (term)
import Minimal.Pretty exposing (ppTerm)
import Minimal.Syntax exposing (AttrName, AttrValue(..), Object, Term(..))
import Helper.Graph exposing (setEdge)
import Helper.Graph exposing (EdgeLabel)
import Helper.Graph exposing (Graph)


type alias State =
    { graph : G.Graph

    -- id of subgraph's parent node
    , currentId : Int

    -- last used (maximal) id of a node
    , maxId : Int
    }


{-| select subgraph building rule based on term's structure-}
toGraph : Term -> State -> State
toGraph term state =
    case term of
        Object object ->
            rule1 object state

        Dot t a ->
            rule2 t a state

        App t1 ( a, t2 ) ->
            rule3 t1 a t2 state

        Locator n ->
            rule4 n state

{-| add a subgraph for value of an object's another attribute
-}
combine : ( AttrName, AttrValue ) -> State -> State
combine ( name, value ) state =
    let
        -- take the first available id for a new node
        -- created for attribute's value
        id =
            state.maxId + 1

        -- attribute name becomes edge label
        label =
            name

        -- take current graph
        g =
            state.graph

        -- select edge style
        edgeType =
            case value of
                Attached v ->
                    case v of
                        Object _ ->
                            G.Solid

                        Locator _ ->
                            G.Solid

                        Dot _ _ ->
                            G.Solid

                        App _ _ ->
                            G.Dashed

                Void ->
                    G.Solid
        edge = G.Edge state.currentId id label edgeType

        -- add new edge to it
        g1 =
            G.setEdge edge g

        -- updated state for recursion into value's branch
        s1 =
            { state | graph = g1, currentId = id, maxId = id }
    in
    case value of
        Attached term ->
            let
                g2 =
                    toGraph term s1
            in
            -- since we combine branches of same-level _children_
            -- set back parent id
            { g2 | currentId = state.currentId }

        Void ->
            -- can return graph with just an added edge
            { state | graph = g1 }


{-| rule for: [a1->t1, a2->t2, ..., an->tn]
add subgraphs for all values of an object's attributes
-}
rule1 : Object -> State -> State
rule1 object state =
    List.foldl combine state (Dict.toList object)

{-| rule for: t.a
-}
rule2 : Term -> AttrName -> State -> State
rule2 term name state =
    let
        -- have a solid edge to t.a

        -- set square node for .a
        s1 = 
            let
                node = G.Node state.currentId (G.Label ("."  ++ name)) G.Rectangle
            in
            {state | graph = G.setNode node state.graph}
        
        -- add solid edge with label _a_ to a new node for term _t_
        s2 = 
            let
                id = state.maxId + 1
                edge = G.Edge state.currentId id name G.Solid
                g = G.setEdge edge s1.graph
            in
            {s1 | graph = g}
    in
    toGraph term s2

addEdge : EdgeLabel -> EdgeType -> State ->  State
addEdge label edgeType state =
    let
        from = state.currentId
        to = from + 1
        edge = G.Edge from to label edgeType
        g = G.setEdge edge state.graph
    in
        {state | graph = g, maxId = to}

    
{-| rule for t1(a->t2)-}
rule3 : Term -> AttrName -> Term -> State -> State
rule3 t1 name t2 state =
    let
        -- have edge to t1(a->t2)

        -- add solid edge with label _a_ to a new node for term _t2_ 
        s1 = addEdge name G.Solid state
        s2 = addEdge
        id1 = state.maxId + 1
        e1 = G.Edge state.currentId id1 name G.Solid
        g1 = G.setEdge e1 state.graph

        -- since it's an application
        -- add dashed edge without a label to a new node for term _t1_ 
        id2 = id1 + 1
        e2 = G.Edge state.currentId id2 name G.Solid
        g2 = G.setEdge e1 state.graph
    in
    
    s


rule4 : Int -> State -> State
rule4 _ s =
    s
