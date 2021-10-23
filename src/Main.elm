port module Main exposing (main)

{-| Here be module documentation!


# Main

@docs main

-}

import Browser
import Full.Examples exposing (Example)
import Full.PrettyASCII
import Helper.Phi as Phi
import Html exposing (Html, div, p, pre, text, b)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Minimal.GraphBuilder exposing (parseToDotString)
import Minimal.Examples
import Minimal.Pretty

port codeUpdateReceiver : (String -> msg) -> Sub msg
port switchMode : (Int -> msg) -> Sub msg


port replaceCodeWith : String -> Cmd msg
port updateGraph : String -> Cmd msg

-- MAIN


{-| TODO: write documentation
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snippet : String
    , feedback : String
    , mode : Phi.Mode
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { snippet = "", feedback = "", mode = Phi.MinimalPhi }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | Example String
    | Toggle Int

getDot : Phi.Mode -> String -> String
getDot mode s = 
    case mode of
        Phi.FullPhi ->
            "digraph g {\"Phi.Full\n is not yet\n supported\"}"

        Phi.MinimalPhi ->
            if String.isEmpty s
                then "digraph g {\"Graph is empty\"}"
                else parseToDotString s

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newSnippet ->
            -- TODO: make max number of steps configurable
            ( { model | snippet = newSnippet, feedback = Phi.interpretStepsN model.mode 20 newSnippet }
            , updateGraph (getDot model.mode newSnippet)
            )
        
        -- change code in the mirror
        Example raw ->
            ( model, replaceCodeWith raw )

        Toggle i ->
            let
                mode =
                    if i == 1 then
                        Phi.FullPhi

                    else
                        Phi.MinimalPhi

                m1 =
                    { model
                        | mode = mode
                        , feedback = Phi.interpretStepsN mode 20 model.snippet
                    }
            in
            ( m1, Cmd.batch [updateGraph (getDot mode model.snippet)])



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ codeUpdateReceiver Change, switchMode Toggle ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ b [] [text "Dataization "], text "(via term reduction) steps:" ]
        , pre [] [ text model.feedback ]
        , p [] [ b [] [text "Examples "], text"(click on example to try it out):" ]
        , Html.ol [] (viewExamples model)
        ]

viewExamples : Model -> List (Html Msg)
viewExamples model = 
    case model.mode of
        Phi.MinimalPhi -> List.map viewMinimalExample Minimal.Examples.examples
        Phi.FullPhi -> List.map viewFullExample Full.Examples.examples

viewFullExample : Full.Examples.Example -> Html Msg
viewFullExample example =
    Html.li []
        [ Html.code
            [ class "example"
            , title example.description
            , onClick (Example example.raw)
            ]
            [ text (Full.PrettyASCII.ppTerm Phi.pp example.term) ]
        ]

viewMinimalExample : Minimal.Examples.Example -> Html Msg
viewMinimalExample example =
    Html.li []
        [ Html.code
            [ class "example"
            , title example.description
            , onClick (Example example.raw)
            ]
            [ text (Minimal.Pretty.ppTerm example.term) ]
        ]