module Minimal.GraphBuilder exposing (..)

import Dict
import Helper.Graph as G exposing (EdgeLabel, EdgeType(..), NodeFrame(..))
import Html.Attributes exposing (name)
import Minimal.Parser exposing (term)
import Minimal.Syntax exposing (AttrName, AttrValue(..), Object, Term(..))
import Minimal.Parser exposing (parse)
import Url exposing (percentEncode)
type alias State =
    { graph : G.Graph

    -- id of subgraph's parent node
    , currentId : Int

    -- last used (maximal) id of a node
    , maxId : Int
    }

emptyState : State
emptyState = State G.emptyGraph 0 0

{-| select a subgraph building rule based on term's structure
-}
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
        -- update state for recursion into value's branch
        s1 =
            addEdge (Just name) G.Solid state
    in
    case value of
        Attached term ->
            let
                s2 =
                    toGraph term {s1 | currentId = s1.maxId}
            in
            -- since we combine branches of same-level _children_
            -- set back parent id
            { s2 | currentId = state.currentId }

        Void ->
            -- can return graph with just an added edge
            s1


{-| rule for: ⟦ a₁ ↦ ∅, …, aₖ ↦ ∅, b₁ ↦ t₁, …, bₙ ↦ tₙ ⟧

add subgraphs for all values of an object's attributes

-}
rule1 : Object -> State -> State
rule1 object state =
    List.foldl combine state (Dict.toList object)


type alias NodeLabel =
    String


{-| update current node with given _.attribute_ and _frame style_

circle nodes drop label

-}
setNode : NodeLabel -> NodeFrame -> State -> State
setNode name frame state =
    let
        label =
            case frame of
                Point ->
                    Nothing

                Rectangle ->
                    Just name

        node =
            G.Node state.currentId label frame
    in
    { state | graph = G.setNode node state.graph }


{-| create a new edge with given label and type from current node

preserve current id and update max id

-}
addEdge : EdgeLabel -> EdgeType -> State -> State
addEdge name edgeType state =
    let
        from =
            state.currentId

        to =
            state.maxId + 1

        edge =
            G.Edge from to name edgeType

        g =
            G.setEdge edge state.graph
    in
    { state | graph = g, maxId = to }

-- TODO: if Dot and parent Squre, setNode

{-| rule for: t.a
-}
rule2 : Term -> AttrName -> State -> State
rule2 term name state =
    let
        -- have a solid edge to t.a
        -- set square node for .a
        -- s1 =
        --     setNode ("." ++ name) Rectangle state

        -- add solid edge without label to a new node for term _t_
        (s2, t2) = 
            case term of
                --  if term is a dot, we can append current access to previous one
                Dot t1 name1 -> 
                    (rule2 t1 (name1 ++ "." ++ name) state, Locator 0)
                Locator n -> 
                    (setNode (getLocatorLabel n ++ "." ++ name) Rectangle state, Locator 0)
                t1 -> (setNode ("." ++ name) Rectangle
                    (addEdge Nothing Solid state), t1)
        s3 =
            case t2 of
                Locator _ -> s2
                _ -> toGraph t2 {s2 | currentId = s2.maxId}
    in
        s3    


{-| rule for: t₁(a ↦ t₂)
-}
rule3 : Term -> AttrName -> Term -> State -> State
rule3 t1 name t2 state =
    let
        -- have edge to t₁(a ↦ t₂)

        -- add solid edge with label _a_ to a new node for term _t2_
        s1 =
            addEdge (Just name) Solid state

        -- build subgraph for _t2_ with new node's id
        s2 =
            toGraph t2 { s1 | currentId = s1.maxId }

        -- since it's an application to _t1_
        -- add dashed edge without a label to a new node for term _t1_
        s3 =
            addEdge Nothing Dashed {s2 | currentId = state.currentId}

        -- build subgraph for _t1_ with new node's id
        s4 =
            toGraph t1 { s3 | currentId = s3.maxId }
    in
    s4



getLocatorLabel : Int -> String
getLocatorLabel locator =
    case locator of
        0 -> "ξ"
        n -> "ρ<SUP>" ++ (String.fromInt n) ++ "</SUP>"

{-| rule for: ρⁿ
-}

rule4 : Int -> State -> State
rule4 n state =
    let
        -- have edge to ρⁿ
                
        -- set square node with pretty locator
        s1 = setNode (getLocatorLabel n) Rectangle state
    in
    s1


termToDotString : Term -> String
termToDotString term =
    let
        state = toGraph term emptyState
        s = G.getDOT state.graph
    in
    s

parseToDotString : String -> String
parseToDotString s = 
    let
        term = 
            case parse s of
                Ok t -> t
                Err _ -> Locator 0 

        state = toGraph term emptyState
        s1 = G.getDOT state.graph
    in
    s1
    
-- TESTS

{-| 
take string with phi term

return dot string
-}

test : String -> String
test = parseToDotString

-- test "[ a->$.t(a->$.b), d->$.a ]"
-- test "[ a->$.t(a->$.b) ]"
-- test "[ a->[b->$.c](a->$.b), d->$.a ]"
-- test "[t->^.a(a1->$.t1)(a2->$.t2).b]"

-- Doesn't render
-- test "[p->[a->?](a->^.d)]"
-- test "[x->[y->^.^.a(a1->$.t1)(a2->$.t2).b.c.d]]"