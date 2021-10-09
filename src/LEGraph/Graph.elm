module LEGraph.Graph exposing (..)

import Browser
import Dict exposing (..)
import Html exposing (Html, div)
import Json.Encode as E
import Minimal.Syntax exposing (Term(..))


-- Take Term, return DOT-string

type alias Model =
    { labelId : Dict String Int
    , edges : Dict ( Int, Int ) ( String, String )
    , nodeCounter : Int
    }


initialModel : Model
initialModel =
    Model (Dict.fromList [ ( "some", 1 ), ( "\"graph\"", 2 ) ]) (Dict.fromList [ ( ( 1, 2 ), ( "default", "dashed" ) ) ]) 3


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Dict.empty Dict.empty 0
    , updateGraph (composeUrlJson initialModel)
    )



-- UPDATE


type Msg
    = Insert D.Value
    | Remove D.Value


type alias InsertEdge =
    { from : Label
    , to : Label
    , label : Label
    , edgeType : EdgeType
    }


insertEdgeDecoder : D.Decoder InsertEdge
insertEdgeDecoder =
    D.succeed InsertEdge
        |> required "from" D.string
        |> required "to" D.string
        |> required "label" D.string
        |> required "type" D.string


{-| Insert a node with label and return its id
-}
addNode : Label -> Model -> ( Model, NodeId )
addNode label model =
    case Dict.get label model.labelId of
        Just id ->
            ( model, id )

        Nothing ->
            ( { model
                | labelId = Dict.insert label model.nodeCounter model.labelId
                , nodeCounter = model.nodeCounter + 1
              }
            , model.nodeCounter
            )


type alias Label =
    String


type alias EdgeType =
    String


type alias EdgeEnds =
    ( NodeId, NodeId )


type alias EdgeInfo =
    ( Label, EdgeType )


addEdge : EdgeEnds -> EdgeInfo -> Model -> Model
addEdge ( v1, v2 ) ( label, edgeType ) model =
    { model | edges = Dict.insert ( v1, v2 ) ( label, edgeType ) model.edges }


insertEdgeHandler : D.Value -> Model -> ( Model, Cmd Msg )
insertEdgeHandler value model =
    case D.decodeValue insertEdgeDecoder value of
        Ok e ->
            let
                ( m1, fromId ) =
                    addNode e.from model

                ( m2, toId ) =
                    addNode e.to m1

                m3 =
                    addEdge ( fromId, toId ) ( e.label, e.edgeType ) m2
            in
            ( m3, updateGraph (composeUrlJson m3))

        Err _ ->
            ( model, Cmd.none )


type alias RemoveEdge =
    { from : Label
    , to : Label
    }


removeEdgeDecoder : D.Decoder RemoveEdge
removeEdgeDecoder =
    D.succeed RemoveEdge
        |> required "from" D.string
        |> required "to" D.string


getWithDefault : Dict comparable a -> a -> comparable -> a
getWithDefault dict default key =
    case Dict.get key dict of
        Just value ->
            value

        Nothing ->
            default


getPairWithDefault : Dict comparable a -> a -> ( comparable, comparable ) -> ( a, a )
getPairWithDefault dict default ( p1, p2 ) =
    let
        defaultGet =
            getWithDefault dict default
    in
    Tuple.mapBoth defaultGet defaultGet ( p1, p2 )


composeUrlJson : Model -> E.Value
composeUrlJson model =
    let
        idLabel =
            Dict.fromList (List.map (\( k, v ) -> ( v, k )) (Dict.toList model.labelId))

        styledEdges =
            List.map
                (\( ( v1, v2 ), ( label, edgeType ) ) ->
                    let
                        ( l1, l2 ) =
                            getPairWithDefault idLabel "" ( v1, v2 )
                    in
                    List.foldr (++) "" ["    ", l1, " -> ", l2, "[ label = ", "\"   ", label, "\"", ", style = ", edgeType, " ];\n" ]
                )
                (Dict.toList model.edges)

        graph =
            List.foldr (++) "" [ "digraph D {\n", List.foldr (++) "" styledEdges, "}" ]
    in
    E.object
        [ ( "dot", E.string graph )
        ]


removeEdgeHandler : D.Value -> Model -> ( Model, Cmd Msg )
removeEdgeHandler value model =
    case D.decodeValue removeEdgeDecoder value of
        Ok e ->
            let
                ( v1, v2 ) =
                    getPairWithDefault model.labelId -1 ( e.from, e.to )

                newModel =
                    { model | edges = Dict.remove ( v1, v2 ) model.edges }
            in
            ( newModel, updateGraph (composeUrlJson newModel) )

        Err _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Insert edge ->
            insertEdgeHandler edge model

        Remove edge ->
            removeEdgeHandler edge model



-- VIEW


view : Model -> Html Msg
view _ =
    div [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ insertEdge Insert, removeEdge Remove ]