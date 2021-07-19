port module Main exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, Attribute, p, code, div, input, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Url.Builder

import Phi
import Phi.Examples exposing (Example)
import Phi.PrettyASCII

port codeUpdateReceiver : (String -> msg) -> Sub msg

port replaceCodeWith : String -> Cmd msg

-- MAIN


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
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { snippet = "", feedback = "" }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Change String
  | Example String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Change newSnippet ->
      -- TODO: make max number of steps configurable
      ( { model | snippet = newSnippet, feedback = Phi.interpretStepsN 20 newSnippet }
      , Cmd.none
      )
    Example raw ->
      ( model, replaceCodeWith raw )


-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  codeUpdateReceiver Change


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text "Dataization (via term reduction) steps:" ]
    , pre [] [ text model.feedback ]
    , p [] [ text "Examples (click on example to try it out):" ]
    , Html.ol [] (List.map viewExample Phi.Examples.examples)
    ]

viewExample : Example -> Html Msg
viewExample example =
  Html.li []
    [ Html.code [
        class "example",
        title example.description,
        onClick (Example example.raw)
        ] [ text (Phi.PrettyASCII.ppTerm Phi.pp example.term) ]
    ]
