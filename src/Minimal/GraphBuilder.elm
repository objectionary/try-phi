module Minimal.GraphBuilder exposing (..)

import Dict
import Helper.Graph as G
import Html.Attributes exposing (name)
import Minimal.Syntax exposing (AttrName, AttrValue(..), Object, Term(..))
import Dict exposing (Dict)
import Minimal.Parser exposing (term)
import Minimal.Pretty exposing (ppTerm)
import Helper.Graph exposing (EdgeType(..))


type alias State =
    { graph : G.Graph

    -- id of parent node
    , parentId : Int

    -- maximal id of a node + 1
    , lastId : Int
    }


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


hasApplication : Term -> Bool
hasApplication term =
    case getAppDotSuffix term of
        (Locator _, t) -> False
        _ -> True

combine : ( AttrName, AttrValue ) -> State  -> State
combine ( name, value ) state =
    let
        -- take the last available id for a new vertex
        -- for attribute's value
        id =
            state.lastId + 1

        -- attribute name becomes edge label
        label =
            name

        -- take current graph
        g =
            state.graph

        -- if value contains application, it's a copy
        edgeType = 
            case value of
                Object _ -> G.Solid
                _ ->
                    if hasApplication value
                        then G.Dashed
                        else G.Solid
        -- add new edge to it
        g1 =
            G.setEdge (G.Edge state.parentId id label edgeType) g

        -- updated state for recursion into value's branch
        s1 =
            { state | graph = g1, parentId = id, lastId = id }
    in
    case value of
        Attached term ->
            let 
                g2 = toGraph term s1
            in
            -- need to recursively build its tree
            -- since we combine branches of children
            -- set back parent id
            {g2 | parentId = state.parentId}

        Void ->
            -- can return graph with just an added edge
            { state | graph = g1 }


{-| rule for: [a1->t1, a2->t2, ..., an->tn]
-}
rule1 : Object -> State -> State
rule1 object state = List.foldl combine state (Dict.toList object)

{-| split a term into 
        prefix that is a locator or ends with an application
        suffix that is just Dot accesses
-}

-- ^.a().b ->  

getAppDotSuffix : Term -> (Term, Term)
getAppDotSuffix term = 
    case term of
    -- TODO: Nothing or Just Term
        -- dirty hack
        Locator n -> 
            (Locator n, Locator -1)
        Dot (Locator n) a ->
            (Locator n, a)
        Dot (App a b) c ->
            (App a b, c)
        -- try to enlarge suffix
        Dot p a ->
            let
                (t, suff) = getAppDotSuffix p
            in
                (t, Dot suff a)

            

{-| rule for: t.a
-}
rule2 : Term -> AttrName -> State -> State
rule2 term name state =
    let
        g = state.graph
        s1 = 
            case term of 
                -- if locator, put it into a square node
                Locator _ -> 
                    let
                        node = G.Node state.parentId (ppTerm term) G.Square
                    in
                    {state | graph = G.setNode node g}
                -- else if it's a Dot, 
                Dot t a -> 
                    let 
                        -- we find largest Dot-suffix and put it into
                        (p, s) = getAppDotSuffix (Dot t a)

                    in

    in
    


rule3 : Term -> AttrName -> Term -> State -> State
rule3 _ _ _ s =
    s


rule4 : Int -> State -> State
rule4 _ s =
    s
