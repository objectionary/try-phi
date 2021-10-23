module Minimal.GraphBuilder exposing (..)

import Dict
import Helper.Graph as G exposing (EdgeLabel, EdgeType(..), NodeFrame(..))
import Html.Attributes exposing (name)
import Minimal.Parser exposing (term)
import Minimal.Syntax exposing (AttrName, AttrValue(..), Object, Term(..))
import Minimal.Parser exposing (parse)
import Dict exposing (Dict)
type alias State =
    { graph : G.Graph

    -- id of subgraph's parent node
    , currentId : Int

    -- last used (maximal) id of a node
    , maxId : Int
    }

emptyState : State
emptyState = State G.emptyGraph 0 0

initialState : State
initialState = setNode 0 " " G.Circle emptyState

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
                    toGraph term {s1 | currentId = s1.maxId}
            in
            -- since we combine branches of same-level _children_
            -- set back parent id
            { s2 | currentId = state.currentId }

        Void ->
            setNode (s1.maxId) "" G.Triangle s1


{-| rule for: ⟦ a₁ ↦ ∅, …, aₖ ↦ ∅, b₁ ↦ t₁, …, bₙ ↦ tₙ ⟧

add subgraphs for all values of an object's attributes

-}
rule1 : Object -> State -> State
rule1 object state = 
    let
        s1 = setNode state.currentId "" Circle state
        s2 = List.foldl combine s1 (Dict.toList object)
    in
    s2
    


type alias NodeLabel =
    String

type alias NodeId = Int

{-| update current node with given _.attribute_ and _frame style_

put node id
-}
setNode : NodeId -> NodeLabel -> NodeFrame -> State -> State
setNode id name frame state =
    let
        label =
            case frame of
                Square ->
                    name
                _ ->
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
    in
    { state | graph = g, maxId = to }


{-| rule for: t.a
-}
rule2 : Term -> AttrName -> State -> State
rule2 term name state =
    let
        -- have a solid edge to t.a

        -- add solid edge without label to a new node for term _t_
        (s2, t2) = 
            case term of
                --  if term is a dot, we can append current access to previous one
                Dot t1 name1 -> 
                    (rule2 t1 (name1 ++ "." ++ name) state, Locator 0)
                Locator n -> 
                    (setNode state.currentId (getLocatorLabel n ++ "." ++ name) Square state, Locator 0)
                t1 -> (setNode state.currentId ("." ++ name) Square
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
        
        -- set current node id
        s1 = 
            setNode state.currentId "" Circle state

        -- add solid edge with label _a_ to a new node for term _t2_
        s2 =
            addEdge name Solid s1

        -- build subgraph for _t2_ with new node's id
        s3 =
            toGraph t2 { s2 | currentId = s2.maxId }

        -- since it's an application to _t1_
        -- add dashed edge without a label to a new node for term _t1_
        s4 =
            addEdge "" Dashed {s3 | currentId = state.currentId}

        -- build subgraph for _t1_ with new node's id
        s5 =
            toGraph t1 { s4 | currentId = s4.maxId }
    in
    s5

sup : Dict Char Char
sup = 
    Dict.fromList 
        [
            ('0', '⁰'),
            ('1', '¹'),
            ('2', '²'),
            ('3', '³'),
            ('4', '⁴'),
            ('5', '⁵'),
            ('6', '⁶'),
            ('7', '⁷'),
            ('8', '⁸'),
            ('9', '⁹')
        ]

getWithDefault : Dict comparable b -> comparable -> b -> b
getWithDefault dict c default = 
    case Dict.get c dict of
        Just c1 -> c1
        Nothing -> default

numberSup : String -> String
numberSup s =
    List.foldr (\a b -> b ++ (String.fromChar (getWithDefault sup a ' '))) "" (String.toList s)

getLocatorLabel : Int -> String
getLocatorLabel locator =
    case locator of
        0 -> "ξ"
        n -> "𝛒" ++ numberSup (String.fromInt n)

{-| rule for: ρⁿ
-}

rule4 : Int -> State -> State
rule4 n state =
    let
        -- have edge to ρⁿ
                
        -- set square node with pretty locator
        s1 = setNode state.currentId (getLocatorLabel n) Square state
    in
    s1


parseToDotString : String -> String
parseToDotString s = 
    let
        term = 
            case parse s of
                Ok t -> t
                Err _ -> Locator 0 

        state = toGraph term initialState
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
-- test "[p->[a->?](a->^.d)]"
-- test "[x->[y->^.^.a(a1->$.t1)(a2->$.t2).b.c.d]]"