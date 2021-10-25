module Minimal.GraphBuilder exposing (..)

import Dict exposing (Dict)
import Helper.Graph as G exposing (EdgeLabel, EdgeType(..), NodeFrame(..))
import Html.Attributes exposing (name)
import Minimal.Parser exposing (parse, term)
import Minimal.Syntax exposing (AttrName, AttrValue(..), Object, Term(..))


type alias State =
    { graph : G.Graph

    -- id of subgraph's parent node
    , currentId : Int

    -- last used (maximal) id of a node
    , maxId : Int
    }


emptyState : State
emptyState =
    State G.emptyGraph 0 0


initialState : State
initialState =
    setNode 0 G.Circle emptyState


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
            addEdge name G.Solid state
    in
    case value of
        Attached term ->
            let
                s2 =
                    toGraph term { s1 | currentId = s1.maxId }
            in
            -- since we combine branches of same-level _children_
            -- set back parent id
            { s2 | currentId = state.currentId }

        Void ->
            setNode s1.maxId G.Square s1


{-| rule for: ⟦ a₁ ↦ ∅, …, aₖ ↦ ∅, b₁ ↦ t₁, …, bₙ ↦ tₙ ⟧

add subgraphs for all values of an object's attributes

-}
rule1 : Object -> State -> State
rule1 object state = List.foldl combine state (Dict.toList object)


type alias NodeLabel =
    String


type alias NodeId =
    Int


{-| update current node with given _.attribute_ and _frame style_

put node id

-}
setNode : NodeId -> NodeFrame -> State -> State
setNode id frame state =
    let
        label =
            String.fromInt id

        node =
            G.Node id label frame
    in
    { state | graph = G.setNode node state.graph }


{-| set edge with given label and type from current node

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
        
        s1 = { state | graph = g, maxId = to }

        s2 = setNode to Circle s1
    in
    s2


{-| rule for: t.a
-}
rule2 : Term -> AttrName -> State -> State
rule2 term name state =
    let
        -- have an edge to t.a
        -- add solid edge with access label to a new node for .a
        s1 =
            addEdge ("." ++ name) G.Solid state

        s2 =
            toGraph term { s1 | currentId = s1.maxId }
    in
    s2


{-| rule for: t₁(a ↦ t₂)
-}
rule3 : Term -> AttrName -> Term -> State -> State
rule3 t1 name t2 state =
    let
        -- have an edge to t₁(a ↦ t₂)

        -- add solid edge with label _a_ to a new node for term _t2_
        s1 =
            addEdge name Solid state

        -- build subgraph for _t2_ with new node's id
        s2 =
            toGraph t2 { s1 | currentId = s1.maxId }

        -- since it's an application to _t1_
        -- add dashed edge without a label to a new node for term _t1_
        s3 =
            addEdge "" Dashed { s2 | currentId = state.currentId }

        -- build subgraph for _t1_ with new node's id
        s4 =
            toGraph t1 { s3 | currentId = s3.maxId }
    in
    s4


sup : Dict Char Char
sup =
    Dict.fromList
        [ ( '0', '⁰' )
        , ( '1', '¹' )
        , ( '2', '²' )
        , ( '3', '³' )
        , ( '4', '⁴' )
        , ( '5', '⁵' )
        , ( '6', '⁶' )
        , ( '7', '⁷' )
        , ( '8', '⁸' )
        , ( '9', '⁹' )
        ]


getWithDefault : Dict comparable b -> comparable -> b -> b
getWithDefault dict c default =
    case Dict.get c dict of
        Just c1 ->
            c1

        Nothing ->
            default


numberSup : String -> String
numberSup s =
    List.foldr (\a b -> b ++ String.fromChar (getWithDefault sup a ' ')) "" (String.toList s)


getLocatorLabel : Int -> String
getLocatorLabel locator =
    case locator of
        0 ->
            "ξ"

        n ->
            "𝜌" ++ numberSup (String.fromInt n)


{-| rule for: ρⁿ
-}
rule4 : Int -> State -> State
rule4 n state =
    let
        -- have edge to ρⁿ
        -- add new edge with pretty locator label
        s1 =
            addEdge (getLocatorLabel n) Solid state

        -- set square node with pretty locator
    in
    s1


parseToDotString : String -> String
parseToDotString s =
    let
        term =
            case parse s of
                Ok t ->
                    t

                Err _ ->
                    Locator 0

        state =
            toGraph term initialState

        s1 =
            G.getDOT state.graph
    in
    s1



-- TESTS


{-| take string with phi term

return dot string

-}
test : String -> String
test =
    parseToDotString



-- test "[ a->$.t(a->$.b), d->$.a ]"
-- test "[ a->$.t(a->$.b) ]"
-- test "[ a->[b->$.c](a->$.b), d->$.a ]"
-- test "[t->^.a(a1->$.t1)(a2->$.t2).b]"
-- test "[p->[a->?](a->^.d)]"
-- test "[x->[y->^.^.a(a1->$.t1)(a2->$.t2).b.c.d]]"
