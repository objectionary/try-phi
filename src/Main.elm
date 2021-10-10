port module Main exposing (main)

{-| Here be module documentation!

# Main
@docs main

-}

import Browser
import Html exposing (Html, Attribute, p, code, div, input, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Url.Builder

import Helper.Phi as Phi
import Full.Examples exposing (Example)
import Full.PrettyASCII
import Platform.Cmd exposing (batch)

port codeUpdateReceiver : (String -> msg) -> Sub msg

port replaceCodeWith : String -> Cmd msg

port switchMode : (Int -> msg) -> Sub msg

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
  ( { snippet = "", feedback = "", mode = Phi.FullPhi }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Change String
  | Example String
  | Toggle Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Change newSnippet ->
      -- TODO: make max number of steps configurable
      ( { model | snippet = newSnippet, feedback = Phi.interpretStepsN model.mode 20 newSnippet }
      , Cmd.none
      )
    Example raw ->
      ( model, replaceCodeWith raw )
    -- decode string and pattern match
    -- create new subscription
    Toggle i ->
      let
          mode = if i == 1 then Phi.FullPhi else Phi.MinimalPhi
      in
        ({model | mode = mode, feedback = Phi.interpretStepsN mode 20 model.snippet}, 
        Cmd.none)
    



-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [codeUpdateReceiver Change, switchMode Toggle]



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text "Dataization (via term reduction) steps:" ]
    , pre [] [ text model.feedback ]
    , p [] [ text "Examples (click on example to try it out):" ]
    , Html.ol [] (List.map viewExample Full.Examples.examples)
    ]

viewExample : Example -> Html Msg
viewExample example =
  Html.li []
    [ Html.code [
        class "example",
        title example.description,
        onClick (Example example.raw)
        ] [ text (Full.PrettyASCII.ppTerm Phi.pp example.term) ]
    ]